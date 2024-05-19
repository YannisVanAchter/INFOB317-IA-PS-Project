import { useState } from "react";

const useParams = () => {
    const [params, setParams] = useState([
        { id: 0, isHuman: false },
        { id: 1, isHuman: false },
        { id: 2, isHuman: false },
        { id: 3, isHuman: false }
    ]);

    const setParam = (key: number, value: boolean) => {
        setParams(params.map((param) => {
            if (param.id === key) {
                return { id: key, isHuman: value };
            }
            return param;
        }));
    }

    return { params, setParam };
}

export { useParams };