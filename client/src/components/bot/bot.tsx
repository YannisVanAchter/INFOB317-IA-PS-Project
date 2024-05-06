import React, { useState, useEffect } from 'react';
import axios from 'axios';

import './bot.css';

interface Message {
    id: number;
    content: string;
    sender: 'user' | 'bot';
    response?: Message;
}

function ChatBot(props: any) {
    const firstMessage: Message = {
        id: 0,
        content: "Bonjour, comment puis-je vous aider?",
        sender: 'bot'
    };
    const [messages, setMessages] = useState<Message[]>([firstMessage]);
    const [input, setInput] = useState('');
    const [showChat, setShowChat] = useState(false);
    const [scrollHeight, setScrollHeight] = useState(0);

    const toggleChat = () => {
        setShowChat(!showChat);
    };

    const updateMessages = (newMessage: Message) => {
        setMessages([...messages, newMessage]);
    }

    const sendMessage = async (event: any) => {
        event.preventDefault();
        // Using async function to send message to the server
        // Allow user to send multiple messages without refreshing the page
        // And getting the answer from the bot
        const newMessage: Message = {
            id: messages.length + 1,
            content: input,
            sender: 'user',
        };
        updateMessages(newMessage);
        setInput('');

        try {
            const url = `${process.env.REACT_APP_SERVER_URL}/message`
            console.log('url:', url);
            const response = await axios.get(url, {
                params: {
                    question: input,
                },
            });
            const botResponse: Message = {
                id: messages.length + 2,
                content: response.data.botResponse,
                sender: 'bot',
                response: newMessage,
            };
            updateMessages(botResponse);
        } catch (error) {
            console.error('Error sending message:', error);
        }
    };

     // Effet to adjust the scroll height of the chat
    useEffect(() => {
        const messagesElement = document.querySelector('.messages');
        if (messagesElement) {
            setScrollHeight(messagesElement.scrollHeight);
        }
    }, [messages]); 

    // Effet to scroll to the bottom of the chat
    useEffect(() => {
        const messagesElement = document.querySelector('.messages');
        if (messagesElement && scrollHeight > 0) {
            messagesElement.scrollTop = scrollHeight;
        }
    }, [scrollHeight]); 

    return (
        <>
            <button onClick={toggleChat} className={`display-button ${!showChat? 'show fade-in' : 'hide fade-out'}`}>J'ai une question!</button>
            <div className={`chat-bot ${showChat? 'show fade-in' : 'hide fade-out'}`}>
                <div className="header">
                    <h3>Chat Bot</h3>
                    <button 
                        onClick={(e) => setMessages([firstMessage])} 
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
                        onChange={(e) => setInput(e.target.value)}
                        placeholder="Tape ton message..."
                    />
                    <input type="submit" value="Send" />
                </form>
            </div>
        </>
    );
};

export default ChatBot;
