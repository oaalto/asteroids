# Asteroids

A classic old-school Asteroids game built in Elm, rendered on an HTML5 canvas.

## Prompt

> This is an empty Elm project. Create a simple old-school Asteroids game. Use the canvas element to draw the graphics. Use simple geometric shapes filled with different colors. Keyboard for input. The game should have start state, game state and end state and a proper gameloop.

## Screenshot

```
         ____  ___________________  ____  ________  _____
        /    |/   _____/\__    ___\/    |/   __   \/  _  \
       /     |\_____  \   |    |  /     |\  ___   /  /_\  \
      /  |   |/        \  |    | /  |   | \   \  /    |    \
      \__|___/_________/  |____| \__|___| /___/ /\____|__  /
                                                         \/
```

## How to Play

- **Arrow Left / Right** — Rotate ship
- **Arrow Up** — Thrust
- **Space** — Shoot
- **Enter** — Start game / Restart after game over

## Game States

| State     | Description                                      |
|-----------|--------------------------------------------------|
| Start     | Title screen with drifting asteroids. Press Enter.|
| Playing   | Fly, shoot, survive. Score points by destroying asteroids. |
| Game Over | Final score displayed. Press Enter to play again. |

## Scoring

| Asteroid Size | Points |
|---------------|--------|
| Large         | 20     |
| Medium        | 50     |
| Small         | 100    |

You start with **3 lives**. Destroying all asteroids advances you to the next level, which spawns more asteroids.

## Building

Requires [Elm 0.19.1](https://elm-lang.org/).

```bash
elm make src/Main.elm --output=elm.js
```

Then open `index.html` in a browser.

## Project Structure

```
.
├── elm.json        # Elm project config and dependencies
├── index.html      # HTML host page with canvas renderer (JS)
├── src/
│   └── Main.elm    # All game logic (model, update, subscriptions, ports)
└── elm.js          # Compiled output (generated)
```

## Architecture

All game logic — physics, collisions, state transitions, scoring — lives in Elm. The Elm app sends a JSON description of each frame to JavaScript via a port. The JS side draws everything onto a `<canvas>` element using simple geometric shapes:

- **Ship** — Filled polygon with a cockpit dot and a flickering thrust flame
- **Asteroids** — Irregular filled polygons in three sizes and colors
- **Bullets** — Small glowing circles
- **Particles** — Fading dots for explosions and thrust exhaust

A subtle starfield and scanline overlay complete the retro look.
