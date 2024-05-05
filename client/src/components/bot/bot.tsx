import React, { useState } from 'react';
import axios from 'axios';

import './bot.css';

interface Message {
    id: number;
    content: string;
    sender: 'user' | 'bot';
    response?: Message;
}

function ChatBot(props: any) {

    const [messages, setMessages] = useState<Message[]>([{ 
        id: 0, 
        content: "Bonjour, comment puis-je vous aider ?", 
        sender: 'bot' 
    }]);
    const [input, setInput] = useState('');
    const [showChat, setShowChat] = useState(false);

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

    return (
        <>
            <button onClick={toggleChat} className={`display-button ${!showChat ? 'show' : 'hide'}`}>Toggle Chat</button>
            <div className={`chat-bot ${showChat ? 'show' : 'hide'}`}>
                <div className="header">
                    <h3>Chat Bot</h3>
                    <button onClick={toggleChat} className='display-button close'>Close</button>
                </div>
                <ul className="messages">
                    {messages.map((message) => (
                        <li key={message.id} className={`message ${message.sender}`}>
                            {message.content}
                        </li>
                    ))}
                    <li>
                    </li>
                </ul>
                <form onSubmit={sendMessage} className='form-chat-bot'>
                    <input
                        type="text"
                        value={input}
                        onChange={(e) => setInput(e.target.value)}
                        placeholder="Type your message..."
                    />
                    <input type="submit" value="Send" />
                </form>
            </div>

        </>
    );
};

export default ChatBot;
