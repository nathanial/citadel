#!/bin/bash
# Citadel HTTP Server Benchmark Script

set -e

HOST="${HOST:-127.0.0.1}"
PORT="${PORT:-8080}"
DURATION="${DURATION:-10}"
CONNECTIONS="${CONNECTIONS:-10}"
URL="http://${HOST}:${PORT}/"

echo "Citadel Benchmark"
echo "================="
echo "Target: $URL"
echo "Duration: ${DURATION}s"
echo ""

# Check for benchmarking tools
if command -v wrk &> /dev/null; then
    echo "Using wrk (recommended)"
    echo ""
    wrk -t4 -c${CONNECTIONS} -d${DURATION}s "$URL"
    echo ""
    echo "Testing JSON endpoint..."
    wrk -t4 -c${CONNECTIONS} -d${DURATION}s "http://${HOST}:${PORT}/api/status"

elif command -v ab &> /dev/null; then
    echo "Using Apache Bench (ab)"
    echo ""
    REQUESTS=$((DURATION * 1000))
    ab -n $REQUESTS -c ${CONNECTIONS} -k "$URL"

elif command -v hey &> /dev/null; then
    echo "Using hey"
    echo ""
    hey -z ${DURATION}s -c ${CONNECTIONS} "$URL"

else
    echo "No benchmark tool found. Using curl (less accurate)."
    echo "For better results, install: brew install wrk"
    echo ""

    # Simple curl-based benchmark
    START=$(date +%s.%N)
    COUNT=0
    END_TIME=$(($(date +%s) + DURATION))

    while [ $(date +%s) -lt $END_TIME ]; do
        curl -s "$URL" > /dev/null &
        COUNT=$((COUNT + 1))
        # Limit concurrent requests
        if [ $((COUNT % CONNECTIONS)) -eq 0 ]; then
            wait
        fi
    done
    wait

    END=$(date +%s.%N)
    ELAPSED=$(echo "$END - $START" | bc)
    RPS=$(echo "scale=2; $COUNT / $ELAPSED" | bc)

    echo "Requests:     $COUNT"
    echo "Time:         ${ELAPSED}s"
    echo "Requests/sec: $RPS"
fi
