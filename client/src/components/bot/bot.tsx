import React, { useState } from 'react';
import axios from 'axios';

import './bot.css';

interface Message {
    id: number;
    content: string;
    sender: 'user' | 'bot';
    response?: Message;
}

function ChatBot() {
    const [messages, setMessages] = useState<Message[]>([]);
    const [input, setInput] = useState('');
    const [showChat, setShowChat] = useState(false);

    const toggleChat = () => {
        setShowChat(!showChat);
    };

    const updateMessages = (newMessage: Message) => {
        setMessages([...messages, newMessage]);
    }

    const sendMessage = async () => {
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
            const response = await axios.post(`${process.env.REACT_APP_SERVER_URL}/message`, {
                content: input,
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
        <div className={`chat-bot ${showChat ? 'show' : 'hide'}`}>
            <button onClick={toggleChat}>Toggle Chat</button>
            <div className="messages">
                {messages.map((message) => (
                    <div key={message.id} className={`message ${message.sender}`}>
                        {message.content}
                    </div>
                ))}
            </div>
            <form onSubmit={sendMessage}>
                <input
                    type="text"
                    value={input}
                    onChange={(e) => setInput(e.target.value)}
                    placeholder="Type your message..."
                />
                <input type="submit" value="Send" />
            </form>
        </div>
    );
};

export default ChatBot;
