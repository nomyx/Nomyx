until Nomyx; do
    echo "Nomyx crashed with exit code $?.  Respawning.." >&2
    sleep 1
done
