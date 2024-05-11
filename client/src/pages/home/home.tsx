import React from "react";

import { GameSettingsModal } from "../../components/gameSettingsModal/gameSettingsModal";

import "./home.css";

function Home() {
    const [showSettingsModal, setShowSettingsModal] = React.useState(false);
    return (
        <div className="home">
            <h1>Bienvenu sur le jeu Tour de France</h1>
            <p>Cliquer sur "nouvelle partie pour accèder au parametre de la partie</p>
            <button onClick={() => setShowSettingsModal(true)}>Nouvelle partie</button>
            {showSettingsModal && <GameSettingsModal />}
        </div>
    );
}

export default Home;