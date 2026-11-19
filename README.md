# Commuter Guide (PUV-TA) 🚌
Plan a jeepney / bus / UV trip. **HTML UI → Node.js API → COBOL fare & ETA engine.**
Includes **Sumbong**: commuters report traffic, accidents and bad roads; those reports feed into the COBOL ETA.

## Architecture
```
Browser (public/index.html)
   │ POST /api/trip {routeId, from, to, passengers, passengerType}
   ▼
Node API (server.js)  - finds stop distances, counts active traffic/accident reports
   │ "T BUSAC 0 14 2 STUDENT 1"  (stdin)
   ▼
COBOL (cobol/farecalc.cob) - validation, fare, 20% discount, rounding, ETA, traffic level
   │ "R|OK|14.00|..."          (stdout)
   ▼
Node → JSON → UI shows fare, ETA, route line
```

## Run (GitHub Codespaces / Linux)
```bash
sudo apt-get update && sudo apt-get install -y gnucobol
npm run build      # compiles COBOL -> cobol/farecalc
npm start          # open port 3000
npm run test:cobol # test COBOL by itself
```

## COBOL business rules
- Fare = base fare (first 4–5 km) + extra km × per-km rate, rounded to ₱0.25
- STUDENT / SENIOR / PWD: 20% discount
- 1–10 passengers; distance > 0 and ≤ 60 km
- ETA = distance ÷ vehicle speed + 0.5 min/km for stops + 3 min per active traffic/accident report (max 30)
- Traffic: 0 reports LIGHT, 1–2 MODERATE, 3+ HEAVY

## API
- `GET /api/routes`
- `POST /api/trip`
- `GET /api/reports?routeId=R03` · `POST /api/reports`
