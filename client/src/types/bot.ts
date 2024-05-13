import exp from "constants";

interface Message {
    id: number;
    content: string;
    sender: 'user' | 'bot';
    response?: Message;
}

export type { Message };