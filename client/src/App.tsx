import React from 'react';
import './App.css';

import { TourDeFrance } from './Game';

import { Client } from 'boardgame.io/react';
import TourDeFranceBoard from './components/board/Board';

const TourDeFranceClient = Client({
  game: TourDeFrance,
  board: TourDeFranceBoard,
});

document.title = 'Tour de France';

function App() {
  const [showSettingsModal] = React.useState(false);
  return (
    <div className="App">
      <TourDeFranceClient />
    </div>
  );
}

export default App;
