
import React from 'react';
import './gameSettingsModal.css';


export function GameSettingsModal(props: any) {
    const { context } = props;
    const [_, setShowSettingsModal] = React.useState(false);
    return (
        <div className='modal'>
            <h2>Game Settings</h2>
            <p>Number of Players: {context.G.players.length}</p>
            <button onClick={() => setShowSettingsModal(false)}>Close</button>
        </div>
    );
}