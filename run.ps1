$day = $args[0]
$part = $args[1]

[console]::OutputEncoding = [Text.Encoding]::Utf8
echo "Running $part of $day"
Get-Content "$day/input.txt" | swipl -q -O -g "time(($day : $part, fail))." "$day/$day.pl"
