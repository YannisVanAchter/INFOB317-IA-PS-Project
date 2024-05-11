import axios from "axios";
import { useState, useEffect } from "react";

import { Message } from "../types/bot";

const useBot = (props: any) => {
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
        newMessage.id = messages.length;
        setMessages(messages => [...messages, newMessage]);
    }

    const sendMessage = async (event: any) => {
        event.preventDefault();
        // Using async function to send message to the server
        // Allow user to send multiple messages without refreshing the page
        // And getting the answer from the bot
        const newMessage: Message = {
            id: messages.length,
            content: input,
            sender: 'user',
        };
        updateMessages(newMessage);
        const url = `${process.env.REACT_APP_SERVER_URL}/bot/${input}`.replaceAll(' ', '-');
        setInput('');

        try {
            const response = await axios.get(url);
            console.log(response)
            const botResponse: Message = {
                id: messages.length,
                content: response.data.answer,
                sender: 'bot',
                response: newMessage,
            };
            updateMessages(botResponse);
        } catch (error) {
            console.error('Error sending message:', error);
        }
    };

    const resetChat = () => {
        setMessages([firstMessage]);
    };

    const handleInputChange = (event: any) => {
        setInput(event.target.value);
    }

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

    return {
        messages,
        input,
        showChat,
        toggleChat,
        sendMessage,
        resetChat,
        handleInputChange,
    };
}

export {useBot};