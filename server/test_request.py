import requests
import json

url = "http://localhost:8080/ia"
data = {
    "deck": [],
    "discard": [],
    "currentPlayer": {
        "playerID": 1
    },
    "players": [
        {
            "playerID": 0,
            "hand": [1, 4, 2, 9, 6],
            "bikes": [
                {"position": "3_A_left", "reduce": 0, "turn": 0},
                {"position": "3_A_left", "reduce": 0, "turn": 0},
                {"position": "3_A_left", "reduce": 0, "turn": 0}
            ]
        },
        {
            "playerID": 1,
            "hand": [12, 9, 7, 11, 9],
            "bikes": [
                {"position": "3_A_left", "reduce": 0, "turn": 0},
                {"position": "3_A_left", "reduce": 0, "turn": 0},
                {"position": "3_A_left", "reduce": 0, "turn": 0}
            ]
        },
        {
            "playerID": 2,
            "hand": [5, 10, 6, 11, 3],
            "bikes": [
                {"position": "3_A_left", "reduce": 0, "turn": 0},
                {"position": "3_A_left", "reduce": 0, "turn": 0},
                {"position": "3_A_left", "reduce": 0, "turn": 0}
            ]
        },
        {
            "playerID": 3,
            "hand": [8, 3, 11, 3, 8],
            "bikes": [
                {"position": "3_A_left", "reduce": 0, "turn": 0},
                {"position": "3_A_left", "reduce": 0, "turn": 0},
                {"position": "3_A_left", "reduce": 0, "turn": 0}
            ]
        }
    ]
}

headers = {
    "Content-Type": "application/json"
}

response = requests.post(url, headers=headers, data=json.dumps(data))

print(response.status_code)
print(response.json())