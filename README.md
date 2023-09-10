<div align="center"> 
  <h3>🧩 F# Tetris 🧩</h3>
  <br>
  <img height=500 src="https://github.com/RyushiAok/Tetris/assets/55625375/c1bde67b-885d-4967-b69b-736600743bac" />
</div>

## 🎈 How to play

```sh
cd src
dotnet run --project Tetris
```

| Key | Action |
| -- | -- |
| **F** | Move Left |
| **A** | Move Right |
| **S** | Move Down |
| **Shift** | Rotate Left |
| **Space** | Rotate Right |
| **E** | Hold |

## ⚙️ Algorithm
### Special Spins
Special spins are achieved by combining rotation around the candidate axis, determined by the tetrimino type, orientation, and rotation direction, with shift movements.

<div align="center"> 
  <img width=400 alt="Rotation around the candidate axis" src="https://github.com/RyushiAok/Tetris/assets/55625375/ce6e522a-b5fc-48af-9baf-4c6e3355a9cf" />
  <div><strong>Rotation around the candidate axis</strong></div>
</div>
