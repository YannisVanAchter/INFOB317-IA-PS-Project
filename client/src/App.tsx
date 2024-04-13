import React from 'react';
import './App.css';

import { TourDeFrance } from './Game';

import { Client } from 'boardgame.io/react';
import TourDeFranceBoard from './components/board/Board';

// TODO: check why this is not working 
// ! process.env.REACT_DEBUG is "undefined" when running "npm start"
// ! Find a way to set the environment variable in the .env file and read it here
const isDebug = process.env.REACT_DEBUG === "true" || true; // TODO: once working, replace "|| true" by "|| false" for production
console.log(`process.env.REACT_DEBUG: ${process.env.REACT_DEBUG}, isDebug: ${isDebug}x`); // TODO: remove this line once working

document.title = 'Tour de France';

function App() {
  const TourDeFranceClient = Client({
    game: TourDeFrance,
    board: TourDeFranceBoard,
    numPlayers: 4,
    debug: isDebug,
  });

  const [showSettingsModal] = React.useState(false);
  return (
    <div className="App">
      <TourDeFranceClient />
    </div>
  );
}

export default App;
