import React from 'react';
import { BrowserRouter, Routes, Route, Link } from 'react-router-dom';

import './App.css';

import Home from './pages/home/home';
import Game from './pages/game/game';

document.title = 'Tour de France';

const NotFound = () => <h1>404 - Not Found</h1>;

function App() {
  return (
    <div className="App">
      <BrowserRouter>
        <nav>
          <ul>
            <li>
              <Link to="/">Home</Link>
            </li>
            <li>
              <Link to="/game">Game</Link>
            </li>
          </ul>
        </nav>
        <Routes>
          {/* <Route path="/" element={<Home />} /> */}
          <Route path="/" element={<Game />} />
          <Route path="*" element={<NotFound />} />
        </Routes>
      </BrowserRouter>
    </div>
  );
}

export default App;
