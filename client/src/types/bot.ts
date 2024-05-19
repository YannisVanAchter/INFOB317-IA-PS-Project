import exp from "constants";

interface Message {
    id: number;
    content: string;
    sender: 'user' | 'bot';
    response?: Message;
}

interface AIMove {
    card: number;
    destination: string;
    origin: string;
}

export type { Message, AIMove };