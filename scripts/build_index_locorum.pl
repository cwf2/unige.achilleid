use Data::Dumper;

open (my $fhi, "<:utf8", "cits.export.tsv");
<$fhi>;

my @rec;
my ($t_loc, $t_phrase, $source, $s_loc);
my @old_field = ("ASDF") x 6;

while (my $line = <$fhi>) {
    chomp $line;
    $line =~ s/"//g;
    $line =~ s/\\//g;
    my @field = split(/\t/, $line);
    
    if ($field[0] ne $old_field[0]) {
        print STDERR "new t_loc: $field[0]\n";
        $t_loc = [$field[0], []];
        push @rec, $t_loc;
        @old_field[1..5] = ("ASDF") x 5;
    }
    
    for ($field[1]) {
        s/^\s+//g;
        s/\s+$//g;
        tr/jv/iu/;
    }
    if ($field[1] ne $old_field[1]) {
        print STDERR " new t_phrase: $field[1]\n";
        $t_phrase = [$field[1], []];
        push @{$t_loc->[1]}, $t_phrase;
        @old_field[2..5] = ("ASDF") x 4;
    }
    if ($field[2] ne $old_field[2]) {
        print STDERR "  new source: $field[2]\n";
        $source = [$field[2], []];
        push @{$t_phrase->[1]}, $source;
        @old_field[3..5] = ("ASDF") x 3;
    }
    if ($field[3] ne $old_field[3]) {
        print STDERR "   new s_loc: $field[3]\n";
        $s_loc = [$field[3], []];
        push @{$source->[1]}, $s_loc;
        @old_field[4..5] = ("ASDF") x 2;
    }
        
    push @{$s_loc->[1]}, join(" ", @field[4,5]);
    
    @old_field = @field;
}
close ($fhi);

open (my $fho, ">:utf8", "index.locorum.html");
select $fho;

print <<END;
<html>
<head>
<meta charset="utf8">
<style type="text/css">
td {
    font-size: small;
    vertical-align: top;
}
td.loc {
    width: 70px;
}
div.phrase {
    padding-left: 2em;
    text-indent: -2em;
}
span.phrase {
    font-weight: bold;
}
span.source {
    font-style: italic;
    padding-left: 1em;
}
span.loc {
    padding-left: 0.25em;
    padding-right: 0.25em;
}
span.note {
    padding-left: 0.25em;
}
</style>
</head>
<body>
<table>
END

for my $t_loc (@rec) {
    print '<tr>';
    print '<td class="loc">';
    print $t_loc->[0];
    print '</td> ';
    print '<td> ';
    
    for my $t_phrase (@{$t_loc->[1]}) {
        print '<div class="phrase">';
        print '<span class="phrase">';
        print $t_phrase->[0];
        print '</span> ';
        
        for my $source (@{$t_phrase->[1]}) {
            print '<span class="source">';
            print $source->[0];
            print '</span> ';
            
            for my $s_loc_id (0..$#{$source->[1]}) {
                print STDERR Dumper(@{$source->[1]});
                $s_loc = $source->[1]->[$s_loc_id];
                print '<span class="loc">';
                print $s_loc->[0];
                
                for my $note (@{$s_loc->[1]}) {
                    $note =~ s/\s+$//;
                    $note =~ s/^\s+//;
                    print '<span class="note">';
                    print $note;
                    print '</span>';
                }                
                unless ($s_loc_id == $#{$source->[1]}) { print ","; }
                print '</span>';
            }
        }
        print '</div>';
    }
    
    print '</td></tr>';
    print "\n";
}

print "</table>\n";
print "</body>\n</html>\n";

close($fho);
