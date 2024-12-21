# Checkers Game Coordinate System and Movement

This document explains how coordinates and piece movements work in the checkers game implementation.

## Board Layout and Coordinates

### Initial Board Setup (`src/Board/Construction.hs`)
The game board is represented as an 8x8 grid where:
- Black pieces start at the top (rows 0-2)
- White pieces start at the bottom (rows 5-7)
- Empty middle rows (3-4)

```haskell
initialBoard = [
    -- Black pieces (top)
    [Just (Piece Black Man), Nothing, ...],  -- row 0
    [Nothing, Just (Piece Black Man), ...],  -- row 1
    [Just (Piece Black Man), Nothing, ...],  -- row 2
    -- Empty middle
    [Nothing, Nothing, ...],                 -- row 3
    [Nothing, Nothing, ...],                 -- row 4
    -- White pieces (bottom)
    [Nothing, Just (Piece White Man), ...],  -- row 5
    [Just (Piece White Man), Nothing, ...],  -- row 6
    [Nothing, Just (Piece White Man), ...]   -- row 7
]
```

### Coordinate Systems

The game uses three coordinate systems that work together:

1. **Board Grid Coordinates** (`src/Types/Common.hs`)
   - Origin (0,0) is at the top-left corner
   - First number is row (vertical), second is column (horizontal)
   - Increases right and down: (row, col)
   - Used in the game logic and board representation

2. **Screen Coordinates** (`src/UI/Shared.hs`)
   - Origin is at the window's top-left corner
   - Uses floating-point numbers for precise positioning
   - Transformed from grid coordinates using `boardToScreenPosition`
   - Takes into account square size, scaling, and board offset

3. **Movement Direction** (`src/Rules/Movement.hs`)
   - Defines how pieces move relative to the grid coordinates
   - Black moves down (increasing row numbers): `playerDirection Black = -1`
   - White moves up (decreasing row numbers): `playerDirection White = 1`

## Movement Rules and Direction

### Basic Movement (`src/Rules/Movement.hs`)
```haskell
isValidDirection (Piece player pisType) (fromRow, _) (toRow, _) =
    case pisType of
        King -> True  -- Kings can move in any direction
        Man  -> case player of
            White -> toRow < fromRow  -- White moves up (decreasing row)
            Black -> toRow > fromRow  -- Black moves down (increasing row)
```

Important points:
- Regular pieces (Man) can only move forward (in their direction)
- Kings can move in any direction
- White pieces move "up" the board (decreasing row numbers)
- Black pieces move "down" the board (increasing row numbers)

## Coordinate Transformation

### Grid to Screen (`src/UI/Shared.hs`)
The `boardToScreenPosition` function handles the transformation from grid coordinates to screen coordinates in these steps:
1. Calculate the actual size of each square on screen (includes scaling)
2. Get the board's offset from the window edge
3. Transform grid coordinates to pixel coordinates
4. Apply the offset to center the board

### Screen to Grid (`src/UI/Shared.hs`)
The `screenToBoardPosition` function converts screen coordinates back to grid positions:
1. Adjusts for board offset
2. Converts pixel coordinates to grid coordinates
3. Validates the position is within the board bounds

## Why This Design Works

1. **Consistent Coordinate System**
   - The board array indices match the screen coordinate system (0,0 at top-left)
   - This makes the coordinate transformation straightforward and intuitive

2. **Movement Direction**
   - Black pieces start at the top (rows 0-2) and move down (increasing rows)
   - White pieces start at the bottom (rows 5-7) and move up (decreasing rows)
   - This matches traditional checkers rules while maintaining coordinate system consistency

3. **UI Rendering**
   - The UI naturally renders the board correctly because the coordinate systems align
   - No need for additional transformations or flipping in the rendering code

## Common Pitfalls

1. **Flipping the Board**
   - Don't flip the initial board setup to match the visual representation
   - The current setup already matches the movement rules and coordinate system

2. **Movement Direction**
   - Remember that the movement direction is relative to the grid coordinates
   - White moving "up" means decreasing row numbers
   - Black moving "down" means increasing row numbers

3. **Coordinate Types**
   - Always use the appropriate coordinate type:
     - `Position` for grid coordinates
     - `UIPosition` for UI grid positions
     - `UIScreenPos` for screen pixel positions

## Related Files

- `src/Board/Construction.hs`: Initial board setup
- `src/Rules/Movement.hs`: Movement rules and direction
- `src/UI/Shared.hs`: Coordinate transformation
- `src/Types/Common.hs`: Coordinate type definitions
- `src/UI/Types.hs`: UI-specific coordinate types
