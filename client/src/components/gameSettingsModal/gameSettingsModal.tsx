
import React from 'react';
import './gameSettingsModal.css';

type TODO = {
    modalSettingsDisplay: {
        showSettingsModal: any;
        setShowSettingsModal: (v: any) => void;
    }
}

export function GameSettingsModal(props: TODO) {
    const { showSettingsModal, setShowSettingsModal } = props.modalSettingsDisplay;
    return (
        <div className='modal'>
            <div className='content'>
                <h2>Paramettre de la partie</h2>
                <p>Nombre de joueur 4 (oblicatoire)</p>
                {/* TODO: add form and place parameters value in a useContext to send it to the page "Game" */}
                <button onClick={() => setShowSettingsModal(!showSettingsModal)}>Close</button>
            </div>
        </div>
    );
}