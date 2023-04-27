# diary-viewer

1. Have `nix` with flakes enabled
2. Enter the dev shell with `nix develop`
3. In `backend`
    * Generate the cabal file with `hpack`
    * Run the API server with `cabal run`
4. In `frontend`
    * Run the Vite server with `yarn dev`
