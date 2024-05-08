import React from 'react';
import { BrowserRouter, Routes, Route, Link } from 'react-router-dom';

import './App.css';
import velo from './assets/velo-icon.png';

import Home from './pages/home/home';
import Game from './pages/game/game';

document.title = 'Tour de France';

const NotFound = () => <h1>404 - Not Found</h1>;

function App() {
  return (
    <div className="App">
      <BrowserRouter>
        <header className='header'>
          <nav className='nav-bar'>
            <Link to="/" className='profile-page link'>
              <img className='icon' src={velo} alt="velo" />
              <h1>Tour de France</h1>
            </Link>
            <Link to="/" className='link'>Home</Link>
            <Link to="/game" className='link'>Game</Link>
          </nav>
        </header>
        
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
