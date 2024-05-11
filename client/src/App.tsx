import React from 'react';
import { BrowserRouter, Routes, Route, Link } from 'react-router-dom';

import './App.css';
import bike from './assets/velo-icon.png';

import Home from './pages/home/home';
import Game from './pages/game/game';

document.title = 'Tour de France';

const NotFound = () => {
  return <>
    <h1>404 - Introuvable</h1>
    <p>Désolé, la page que vous rechercher est introuvable</p>
    <Link to="/">Go Home</Link>
  </>
};

function App() {
  return (
    <div className="App">
      <BrowserRouter>
        <header className='header'>
          <nav className='nav-bar'>
            <Link to="/" className='profile-page link'>
              <img className='icon' src={bike} alt="velo" />
              <h1>Tour de France</h1>
            </Link>
            <Link to="/" className='link'>Home</Link>
            <Link to="/game" className='link'>Jeu</Link>
          </nav>
        </header>
        
        <Routes>
          <Route path="/" element={<Home />} />
          <Route path="/game" element={<Game />} />
          <Route path="*" element={<NotFound />} />
        </Routes>
      </BrowserRouter>
    </div>
  );
}

export default App;
