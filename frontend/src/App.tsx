import { useState, useCallback, useEffect } from 'react'
import Button from '@mui/material/Button'
import './App.css'
import Alert from '@mui/material/Alert';
import AlertTitle from '@mui/material/AlertTitle';
import Container from '@mui/material/Container';
import AppBar from '@mui/material/AppBar';
import Typography from '@mui/material/Typography';
import '@fontsource/roboto/300.css';
import '@fontsource/roboto/400.css';
import '@fontsource/roboto/500.css';
import '@fontsource/roboto/700.css';
import Paper from '@mui/material/Paper';
import Stack from '@mui/material/Stack';
import CssBaseline from '@mui/material/CssBaseline';
import TableContainer from '@mui/material/TableContainer';
import Table from '@mui/material/Table';
import TableHead from '@mui/material/TableHead';
import TableRow from '@mui/material/TableRow';
import TableCell from '@mui/material/TableCell';
import TableBody from '@mui/material/TableBody';
import Card from '@mui/material/Card';
import CardContent from '@mui/material/CardContent';
import List from '@mui/material/List';
import ListItem from '@mui/material/ListItem';
import Grid from '@mui/material/Unstable_Grid2';
import Divider from '@mui/material/Divider';
import TableSortLabel from '@mui/material/TableSortLabel';
import RefreshIcon from '@mui/icons-material/Refresh';
import Fade from '@mui/material/Fade';
import Skeleton from '@mui/material/Skeleton';
import AccordionSummary from '@mui/material/AccordionSummary';
import Accordion from '@mui/material/Accordion';
import AccordionDetails from '@mui/material/AccordionDetails';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';

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

function Entries(props: { entryHeadings: EntryHeading[], selectedEntryDay: Day | null, onSelect: (_: EntryHeading) => void, onRefresh: () => void }) {
  const [recentFirst, setRecentFirst] = useState(true)
  return (
    <div className="entries">
      {props.entryHeadings ?
        <TableContainer component={Paper}>
          <Table>
            <TableHead>
              <TableRow>
                <TableCell>
                  Date
                  <TableSortLabel
                    active={true}
                    direction={recentFirst ? 'desc' : 'asc'}
                    onClick={() => setRecentFirst(!recentFirst)}
                  >
                  </TableSortLabel>
                </TableCell>
                <TableCell>Title</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
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
            </TableBody>
          </Table>
        </TableContainer> :
        <p>Loading...</p>
      }
    </div>
  );
}

function EntryHeading(props: { entryHeading: EntryHeading, isSelected: boolean, onSelect: (_: EntryHeading) => void }) {
  return (
    <TableRow
      selected={props.isSelected}
      hover={true}
      onClick={() => props.onSelect(props.entryHeading)}>
      <TableCell>{props.entryHeading.entryDay}</TableCell>
      <TableCell>{props.entryHeading.entryTitle}</TableCell>
    </TableRow>
  );
}

function EntryViewer(props: { entry: Entry }) {
  return (
    <Card className="entry-content">
      <CardContent>
        <Typography sx={{ fontSize: 14 }} color="text.secondary" gutterBottom>
          {props.entry.entryHeading.entryDay}
        </Typography>
        <Typography variant="h5" component="div">
          {props.entry.entryHeading.entryTitle}
        </Typography>
        <Typography variant="body2">
          {props.entry.entryContent}
        </Typography>
      </CardContent>
    </Card>
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
    <Card className="clashes">
      <CardContent>
        <Typography variant="h5" color="text.secondary">Conflicting entries</Typography>
        <List>
          {props.clashes.map(Clash)}
        </List>
      </CardContent>
    </Card>
  );
}

function Missing(props: { missingDays: Day[] }) {
  return (
    <Accordion className="missing">
      <AccordionSummary
        expandIcon={<ExpandMoreIcon />}
        >
        <Typography>Missing Entries</Typography>
      </AccordionSummary>
      <AccordionDetails>
        <List>
          {props.missingDays.map(day =>
            <ListItem key={day}>{day}</ListItem>
          )}
        </List>
      </AccordionDetails>
    </Accordion>
  );
}

function Diary(props: { diary: Diary, selectedEntry: Entry | null, onSelect: (_: Day) => void, onRefresh: () => void }) {
  if (!props.diary) {
    return (<div className="diary">Loading...</div>);
  }
  const missing = <Missing missingDays={props.diary.diaryMissingDays} />;
  const entries =
    <Entries
      entryHeadings={props.diary.diaryEntryHeadings}
      selectedEntryDay={props.selectedEntry ? props.selectedEntry.entryHeading.entryDay : null}
      onSelect={(entryHeading) => props.onSelect(entryHeading.entryDay)}
      onRefresh={props.onRefresh}
    />;
  const clashes = <Clashes clashes={props.diary.diaryClashes} />;
  return (
    <div className="diary">
      <Refresh onRefresh={props.onRefresh} />
      {
        props.diary.diaryClashes.length === 0 ?
          <>
            <Stack spacing={4}>
              {entries}
              {missing}
            </Stack>
          </> :
          clashes
      }
    </div>
  );
}

function Refresh(props: { onRefresh: () => void }) {
  return (
    <Button
      className="refresh"
      startIcon={<RefreshIcon />}
      onClick={props.onRefresh}
    >
      Refresh
    </Button>
  );
}

function Error(props: { error: Error, onCloseError: () => void }) {
  return (
    <Alert severity="error" onClose={props.onCloseError} >
      <AlertTitle>
        {props.error.title}
      </AlertTitle>
      {props.error.description ?
        <>
          {props.error.description}
        </> :
        null
      }
    </Alert>
  );
}

function Errors(props: { errors: Error[], onCloseError: (_: ErrorTime) => void }) {
  return (
    <>
      {props.errors.map(error =>
        <Error
          key={error.time}
          error={error}
          onCloseError={() => props.onCloseError(error.time)}
        />)}
    </>
  );
}

function ConnectionStatus(props: { connected: boolean, onRetry: () => void }) {
  return (!props.connected ?
    <Alert
      severity="warning"
      className="connection-status"
      action={
        <Button color="inherit" onClick={props.onRetry}>Retry</Button>
      }
    >
      <AlertTitle>Disconnected from the server</AlertTitle>
    </Alert>
    : null
  );
}

let didInit = false;

function App() {

  const [diary, setDiary] = useState<Diary | null>(null);
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
        addError('Could not refresh diary', err.message);
      })
  }, [errors])

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
              addError(data.Left.tag);
            }
          }
        })
        .catch(err => {
          addError('Could not access entry ' + day, err.message);
        });
    }, [errors]
  );

  const addError = useCallback(
    (title: string, description?: string) => {
      setErrors([...errors, { time: Date.now(), title: title, description: description }]);
    }, [errors]
  );

  const closeError = useCallback(
    (errorTime: ErrorTime) => {
      setErrors(errors.filter(error => error.time !== errorTime));
    }, [errors]
  );

  const onSocketMessage = useCallback(
    () => {
      refreshDiary();
      if (selectedEntry) {
        selectEntry(selectedEntry.entryHeading.entryDay);
      }
    }, [selectedEntry, selectEntry]);

  const connectToServer = useCallback(
    () => {
      const socket = new WebSocket('ws://localhost:8001/updates');
      if (serverSocket) {
        serverSocket.close();
      }
      socket.onopen = _ => setServerSocket(socket);
      socket.onmessage = onSocketMessage;
      socket.onclose = _ => setServerSocket(null);
      socket.onerror = event => console.log('Socket connection error', event);
    }, [serverSocket, onSocketMessage]);

  // Initialization: Fetch data and create web socket
  useEffect(() => {
    if (!didInit) {
      didInit = true;
      refreshDiary();
      connectToServer();
    }
  }, []);


  // Upon receiving a message on the socket, the selected entry has to be refreshed.
  // The handler for the onmessage event has to be re-evaluated and re-assigned. 
  useEffect(() => {
    if (serverSocket) {
      serverSocket.onmessage = onSocketMessage;
    }
  }, [selectedEntry]);

  return (
    <>
      <CssBaseline />
      <AppBar position='sticky' sx={{ padding: '1em', marginBottom: '1em' }}>
        <Typography variant="h6" component="div">
          diary-viewer
        </Typography>
      </AppBar>
      <Container>
        <Grid container spacing={4}>
          <Grid xs={12}>
            <Stack spacing={1}>
              <ConnectionStatus connected={serverSocket !== null} onRetry={connectToServer} />
              {errors.length > 0 ? <Errors errors={errors} onCloseError={closeError} /> : null}
            </Stack>
          </Grid>
          <Grid xs={12} sm={12} md={4}>
            {diary ?
              <Diary diary={diary} onSelect={selectEntry} onRefresh={refreshDiary} selectedEntry={selectedEntry} /> :
              null}
          </Grid>
          <Grid xs={12} sm={12} md={8}>
            {selectedEntry ? <EntryViewer entry={selectedEntry} /> : null}
          </Grid>
        </Grid>
      </Container>
    </>
  )
}

export default App
