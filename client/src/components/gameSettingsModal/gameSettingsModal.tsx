
import React from 'react';
import './gameSettingsModal.css';


export function GameSettingsModal(props: any) {
    const [_, setShowSettingsModal] = React.useState(false);
    return (
        <div className='modal'>
            <h2>Paramettre de la partie</h2>
            <p>Nombre de joueur 4 (oblicatoire)</p>
            {/* TODO: add form and place parameters value in a useContext to send it to the page "Game" */}
            <button onClick={() => setShowSettingsModal(false)}>Close</button>
        </div>
    );
}