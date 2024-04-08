import './App.css';
import { client } from 'boardgame.io/react';

import { TourDeFrance } from './Game';
import { Board } from './Board';

const ClientTourDeFrance = client({
  game: TourDeFrance,
  board: Board,
  debug: false,
});

function App() {
  return (
    <div className="App">
      <ClientTourDeFrance playerID="0" />
      <ClientTourDeFrance playerID="1" />
    </div>
  );
}

export default App;
