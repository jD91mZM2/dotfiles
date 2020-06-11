#!/usr/bin/env perl

use strict;

my $found_default_sink = 0;
my $percent = 0;

foreach (split /\n/, `pacmd list-sinks`) {
    $found_default_sink = 1 if /\*/;
    if ($found_default_sink && /([0-9]+)%/) {
        $percent = $1;
        last;
    }
}

my $new;

if ($ARGV[0] eq "up") {
    $new = $percent * 1.25;

    $new = 4 if $new == 0;
    $new = 97 if $new > 100;
} elsif ($ARGV[0] eq "down") {
    $new = 0;
    $new = ($percent + 1) / 1.25 if $percent >= 5;
} else {
    print "./volume.perl [up|down]\n";
    exit;
}

system "pactl", "set-sink-volume", '@DEFAULT_SINK@', int($new) . "%";
