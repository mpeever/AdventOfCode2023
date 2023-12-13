#!/usr/bin/env perl

use diagnostics;
use warnings;
use strict;

use List::Util qw/reduce max min/;
use Data::Dumper;


my @seeds = ();
my %maps = ();
my %ranges = ();

my ($src, $dest, $map_name);

while (<>) {
    next if m/^\s*$/;

    chomp;
    
    if (m/^seeds:/) {
	while(m/\b(\d+)\b/g) {
	    my $seed = $1;
	    push(@seeds, $seed);
	}
	next;
    }

    if (m/^((\w+)-to-(\w+)) \s+ map.+$/x) {
	($map_name, $src, $dest) = ($1, $2, $3);
	$maps{$map_name} = [];

	next;
    }

    if (m/^(\d+)\s+(\d+)\s(\d+)\s*$/) {
	my ($dest_range_start, $src_range_start, $length) = ($1, $2, $3);

	my $mapper = make_mapper(
	    src_range_start => $src_range_start,
	    dest_range_start => $dest_range_start,
	    length => $length
	    );
	
	push(@{$maps{$map_name}}, $mapper);
    }
}

warn "seeds: ", Dumper \@seeds;

my %seed_to_location = ();

foreach my $seed (sort @seeds) {
    my $start = 'seed';
    my $end = 'location';
    
    my $current_value = $seed;
    my $current_src = $start;
    my $current_dest = '';

    while ($current_src ne $end) {
	warn "current src: '${current_src}', current value: '${current_value}'";
	
	my @matching_maps = grep {
	    m/^($current_src)-to/;
	} (keys %maps);

	my $current_map_name = shift @matching_maps;
	$current_dest = $2 if ($current_map_name =~ m/^(\w+)-to-(\w+)/);
	
	warn "looking up ${current_src} '${current_value}' in '${current_map_name}'";

	# remember a map is just an array of lambdas
	my @current_map = @{$maps{$current_map_name}};

	# So we're going to loop through our maps until we can find one that changes our value.
	# when we do, we'll exit our eval via die and go to the next step.
	eval {
	    foreach my $fn (@current_map) {
		my $val = $fn->($current_value);
		# escape the loop if we found a match
		die "$val" if $val != $current_value;
	    }
	} or do {
	    if ($@) {
		warn 'caught $@: ', $@;
		if ($@ =~ m/(\d+)/) {
		    my $val = $1;
		    warn "found a matching mapper for '${current_value}', setting to ${val}";
		    $current_value = $val;
		}
	    }
	};
	warn "current value is now '${current_value}'";
	$current_src = $current_dest;
    }

    $seed_to_location{$seed} = $current_value;
}

warn "seed_to_location: ", Dumper \%seed_to_location;

my @locations = ();
foreach (sort keys %seed_to_location) {
    push(@locations, $seed_to_location{$_});
}

print "Lowest location: ", min(@locations), "\n";



# return a function that maps range to range
sub make_mapper {
    my %params = @_;
    
    my @src_range = ($params{src_range_start}, $params{src_range_start} + $params{length});
    my @dest_range = ($params{dest_range_start}, $params{dest_range_start} + $params{length});

    return sub {
	my $input = shift;

	warn "'${input}' is not between '$src_range[0]' and '$src_range[1]'"
	    and return $input
	    unless $input >= $src_range[0]
	    and $input <= $src_range[1];

	my $radius = $input - $src_range[0];
	
	warn "'${input}' is between '$src_range[0]' and '$src_range[1]'";

	return $dest_range[0] + $radius;
    }
}
