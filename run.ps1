$day = $args[0]
$part = $args[1]
echo "Running $part of $day"
Get-Content "$day/input.txt" | swipl -q -g "time(($part, fail))." "$day/$day.pl"
