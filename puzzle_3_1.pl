#!/usr/bin/env perl

use diagnostics;
use warnings;
use strict;

use List::Util qw/reduce/;
use Data::Dumper;

# Sluuurp!
my @lines = (<>);

chomp @lines;

my @part_numbers = ();

for (my $i = 0; $i <= $#lines; $i++) {
    # find all the numbers on the line
    while ($lines[$i] =~ m/(\d+)/g) {
        my $value = $1; 

        my $start = $-[1];
        my $end = $+[1] - 1;
        my $len = $+[1] - $-[1]; 

        # BEWARE! negative substr indices in Perl count from the RIGHT
        my $strstart = $start == 0 ? $start : $start - 1 ;   # start one character to the left of our number
        my $strlen = $start == 0 ? $len + 1 : $len + 2 ;     # one character to the left, one to the right of our number

        warn "examining ${value} on line ${i}: ${start} - ${end} (strstart = ${strstart}, strlen = ${strlen})";

        if ($i > 0) { # don't walk backwards off the map
            my $above = $lines[$i - 1];
            push(@part_numbers, $value) and next if substr($above, $strstart, $strlen) =~ m/[^\.\d]/o; 
        }
 
        push(@part_numbers, $value) and next if substr($lines[$i], $strstart, $strlen) =~ m/[^\.\d]/o;

        if ($i < $#lines) { # don't walk off the end of the map
            my $below = $lines[$i + 1];
            push(@part_numbers, $value) and next if substr($below, $strstart, $strlen) =~ m/[^\.\d]/o;
        }

        # if we got here, it seems like a mismatch, let's log it for diagnostics
        warn "Rejecting number ${value} on line ${i}...";
        if ($i > 0) { 
            my $above = $lines[$i - 1];
            warn substr($above, $strstart - 1, $strlen + 2);
        }
        warn substr($lines[$i], $strstart - 1, $strlen + 2);
        if ($i < $#lines) { 
            my $below = $lines[$i + 1];
            warn substr($below, $strstart - 1, $strlen + 2) ;
        }
        warn "\n";

    }
}

warn "found values: ", Dumper \@part_numbers;

my $sum = reduce { $a + $b } @part_numbers;

print "Sum of part numbers: ${sum}\n";
