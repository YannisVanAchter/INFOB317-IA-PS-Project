import React from 'react';
import './navbar.css';

export function NavBar(props: any) {
    const [showSettingsModal, setShowSettingsModal] = React.useState(false);
    return (
        <header className='navbar'>
            <h1>Tour De France</h1>
            <button onClick={() => setShowSettingsModal(!showSettingsModal)}>Settings</button>
        </header>
    );
}