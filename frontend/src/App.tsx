import { useState, useCallback, useEffect } from 'react'
import './App.css'

type Day = string;

type EntryHeading = {
  entryDay: Day;
  entryTitle: string;
};

type Entry = {
  entryHeading: EntryHeading;
  entryContent: string;
};

type Clash = EntryHeading[];

type Diary = {
  diaryClashes: Clash[];
  diaryEntryHeadings: EntryHeading[];
  diaryMissingDays: Day[];
};

type ErrorTime = number;

type Error = {
  time: ErrorTime;
  title: string;
  description?: string;
}

function Entries(props: { entryHeadings: EntryHeading[], selectedEntryDay: Day | null, onSelect: (_: EntryHeading) => void }) {
  const [recentFirst, setRecentFirst] = useState(true)
  return (
    <div className="entries">
      <h2>Entries</h2>
      {props.entryHeadings ?
        <table>
          <thead>
            <tr>
              <th>
                Date
                <button onClick={() => setRecentFirst(!recentFirst)}>
                  {recentFirst ? "⬆️" : "⬇️ "}
                </button>
              </th>
              <th>Title</th>
              <th></th>
            </tr>
          </thead>
          <tbody>
            {props.entryHeadings
              .sort((e1, e2) =>
                (recentFirst ? -1 : 1) *
                e1.entryDay.localeCompare(e2.entryDay)
              )
              .map(entryHeading =>
                <EntryHeading
                  key={entryHeading.entryDay + entryHeading.entryTitle}
                  isSelected={props.selectedEntryDay === entryHeading.entryDay}
                  onSelect={props.onSelect}
                  entryHeading={entryHeading} />
              )}
          </tbody>
        </table> :
        <p>Loading...</p>
      }
    </div>
  );
}

function EntryHeading(props: { entryHeading: EntryHeading, isSelected: boolean, onSelect: (_: EntryHeading) => void }) {
  return (
    <tr
      className={`entry-row ${props.isSelected ? "selected" : ""}`}
      onClick={() => props.onSelect(props.entryHeading)}>
      <td>{props.entryHeading.entryDay}</td>
      <td>{props.entryHeading.entryTitle}</td>
    </tr>
  );
}

function EntryViewer(props: { entry: Entry }) {
  return (
    <div className="entry-content">
      <h2>{props.entry.entryHeading.entryTitle}</h2>
      <p>{props.entry.entryContent}</p>
    </div>
  )
}

function Clashes(props: { clashes: Clash[] }) {
  function Clash(clash: Clash) {
    // TODO Check for empty clash
    const clashDay = clash[0].entryDay;
    return (
      <div
        key={clashDay}
        className="clash">
        <h3>{clashDay}</h3>
        <ul>{
          clash.map(entryHeading =>
            <li key={entryHeading.entryDay + '-' + entryHeading.entryTitle}>
              {entryHeading.entryTitle}
            </li>
          )
        }</ul>
      </div>);
  }
  return (
    <div className="clashes">
      <h2>Conflicting entries</h2>
      {props.clashes.map(Clash)}
    </div>
  );
}

function Missing(props: { missingDays: Day[] }) {
  return (
    <div className="missing">
      <h2>Missing Entries</h2>
      <ul>
        {props.missingDays.map(day =>
          <li key={day}>{day}</li>
        )}
      </ul>
    </div>
  );
}

function Diary(props: { diary: Diary, selectedEntry: Entry | null, onSelect: (_: Day) => void }) {
  if (!props.diary) {
    return (<div className="diary">Loading...</div>);
  }
  const missing = <Missing missingDays={props.diary.diaryMissingDays} />;
  const entries =
    <Entries
      entryHeadings={props.diary.diaryEntryHeadings}
      selectedEntryDay={props.selectedEntry ? props.selectedEntry.entryHeading.entryDay : null}
      onSelect={(entryHeading) => props.onSelect(entryHeading.entryDay)} />;
  const clashes = <Clashes clashes={props.diary.diaryClashes} />;
  return (
    <div className="diary">
      {
        props.diary.diaryClashes.length === 0 ?
          <>
            {entries}
            {missing}
          </> :
          clashes
      }
    </div>
  );
}

function Refresh(props: { onRefresh: () => void }) {
  return <button className="refresh" onClick={props.onRefresh}>Refresh</button>;
}

function Error(props: { error: Error, onCloseError: () => void }) {
  return (
    <div className="error">
      <div className="error-title">
        {props.error.title}
      </div>
      {props.error.description ?
        <div className="error-description">
          {props.error.description}
        </div> :
        null
      }
      <button
        className="error-close"
        onClick={props.onCloseError}
      >
        Close
      </button>
    </div>
  );
}

function Errors(props: { errors: Error[], onCloseError: (_: ErrorTime) => void }) {
  return (
    <div className="errors">
      {props.errors.map(error =>
        <Error
          key={error.time}
          error={error}
          onCloseError={() => props.onCloseError(error.time)}
        />)}
    </div>
  );
}

function ConnectionStatus(props: { connected: boolean, onRetry: () => void }) {
  return (!props.connected ?
    <div className="connection-status">
      <p>Disconnected from the server</p>
      <button onClick={props.onRetry}>Retry</button>
    </div>
    : null
  );
}

let didInit = false;

function App() {

  const [diary, setDiary] = useState<Diary>(exampleDiary);
  const [selectedEntry, setSelectedEntry] = useState<Entry | null>(null);
  const [errors, setErrors] = useState<Error[]>([]);
  const [serverSocket, setServerSocket] = useState<WebSocket | null>(null);

  const refreshDiary = useCallback(() => {
    fetch('http://localhost:8001/diary')
      .then(response => response.json())
      .then(data => {
        setDiary(data);
      })
      .catch(err => {
        addError(Date.now(), 'Could not refresh diary', err.message);
      })
  }, [])

  const selectEntry = useCallback(
    (day: Day) => {
      fetch('http://localhost:8001/entry/' + day)
        .then(response => response.json())
        .then(data => {
          console.log(data);
          if (data.Right) {
            setSelectedEntry(data.Right ? data.Right : null);
          } else {
            if (data.Left) {
              addError(Date.now(), data.Left.tag);
            }
          }
        })
        .catch(err => {
          addError(Date.now(), 'Could not access entry ' + day, err.message);
        });
    },
    []
  );

  function addError(time: number, title: string, description?: string) {
    setErrors([...errors, { time: time, title: title, description: description }]);
  };

  function closeError(errorTime: ErrorTime) {
    setErrors(errors.filter(error => error.time !== errorTime));
  }

  function connectToServer() {
    const socket = new WebSocket('ws://localhost:8001/updates');
    if (serverSocket) {
      serverSocket.close();
    }
    socket.onopen = _ => setServerSocket(socket);
    socket.onmessage = event => console.log('Message from server ', event.data);
    socket.onclose = _ => setServerSocket(null);
    socket.onerror = event => console.log('Socket connection error', event);
  }

  // useEffect(refreshDiary, []);
  useEffect(() => {
    if (!didInit) {
      didInit = true;
      refreshDiary();
      connectToServer();
    }
  }, []);

  return (
    <div className="App">
      <ConnectionStatus connected={serverSocket !== null} onRetry={connectToServer} />
      {errors.length > 0 ? <Errors errors={errors} onCloseError={closeError} /> : null}
      <Refresh onRefresh={refreshDiary} />
      <Diary diary={diary} onSelect={selectEntry} selectedEntry={selectedEntry} />
      {selectedEntry ? <EntryViewer entry={selectedEntry} /> : null}
    </div>
  )
}

export default App

const exampleDiary = { "diaryClashes": [[{ "entryDay": "2023-01-02", "entryTitle": "Day2Duplicate" }, { "entryDay": "2023-01-02", "entryTitle": "Day2" }], [{ "entryDay": "2023-01-08", "entryTitle": "Day8foo" }, { "entryDay": "2023-01-08", "entryTitle": "Day8bar" }]], "diaryEntryHeadings": [{ "entryDay": "2023-01-05", "entryTitle": "Day5" }, { "entryDay": "2023-01-02", "entryTitle": "Day2Duplicate" }, { "entryDay": "2023-01-01", "entryTitle": "Day1" }, { "entryDay": "2023-01-03", "entryTitle": "Day3" }, { "entryDay": "2023-01-02", "entryTitle": "Day2" }], "diaryMissingDays": ["2023-01-04"] };
