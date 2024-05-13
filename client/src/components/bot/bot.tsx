import React from 'react';
import { useBot } from '../../hooks';
import './bot.css';

function ChatBot(props: any) {
    const {
        messages,
        input,
        showChat,
        toggleChat,
        sendMessage,
        resetChat,
        handleInputChange,
    } = useBot(props);

    return (
        <>
            <button onClick={toggleChat} className={`display-button ${!showChat? 'show fade-in' : 'hide fade-out'}`}>J'ai une question!</button>
            <div className={`chat-bot ${showChat? 'show fade-in' : 'hide fade-out'}`}>
                <div className="header-bot">
                    <h3>Chat Bot</h3>
                    <button 
                        onClick={resetChat} 
                        className='display-button refresh'
                    >
                        Rafraichir
                    </button>
                    <button onClick={toggleChat} className='display-button close'>Fermer</button>
                </div>
                <ul className="messages">
                    {messages.map((message) => (
                        <li key={message.id} className={`message ${message.sender}`}>
                            {message.content}
                        </li>
                    ))}
                </ul>
                <form onSubmit={sendMessage} className='form-chat-bot'>
                    <input
                        type="text"
                        value={input}
                        onChange={handleInputChange}
                        placeholder="Tape ton message..."
                    />
                    <input type="submit" value="Send" />
                </form>
            </div>
        </>
    );
};

export default ChatBot;
