#!/usr/bin/perl

# -------------------------------------------------------------------
# PROGRAM:      intbas.pl
# AUTHOR:       James Pujals (a.k.a. DZ-Jay)
# DESCRIPTION:  Compiles a BASIC source file using IntvBasic compiler
#               and then assembles it using AS-1600 assembler.
# LAST UPDATED: 2014-10-24
#
# LICENSE:      This crappy "program" has been released to the public
#               domain by the author.  No claims of ownership are
#               made.  In fact, I regret putting my name on it. ;)
# -------------------------------------------------------------------

use Cwd;

my $PROLOGUE = 'intybasic_prologue.asm';
my $EPILOGUE = 'intybasic_epilogue.asm';            # MAKE SURE TO CONFIGURE THESE:
my $SDK_PATH = $ENV{'INTV_SDK_PATH'  };             #   Base path to the SDK-1600 installation.
my $BAS_PATH = $ENV{'INTV_BASIC_PATH'};             #   Base path to the IntyBasic installation.
my $LIB_PATH = $ENV{'INTV_BASIC_PATH'} . "/lib";    #   Location of prologue and epilogue files.

# -------------------------------------------------------------------
# PROCESS INPUT
# -------------------------------------------------------------------
my $src_path = shift(@ARGV) or usage();
my ($path,
    $fullname,
    $name,
    $ext)    = ($src_path =~ m/^(.+\/)?(([^\/]+)\.([\w]+))$/i);
my $asm_dir  = $path . "asm";
my $bin_dir  = $path . "bin";
my $asm_file = "${ name }.asm";

# -------------------------------------------------------------------
# VALIDATE INPUT
# -------------------------------------------------------------------
    usage('Bad extension' ) unless ($ext eq 'bas');
    usage('Bad file path' ) unless ($fullname);
    usage('file not found') unless (!$path || -e $path);

    usage("Prologue not found in library path \"${ LIB_PATH }\"") unless (-e "${ LIB_PATH }/${ PROLOGUE }");
    usage("Epilogue not found in library path \"${ LIB_PATH }\"") unless (-e "${ LIB_PATH }/${ EPILOGUE }");
    check_dirs($asm_dir, $bin_dir);

# -------------------------------------------------------------------
# COMPILE BASIC PROGRAM SOURCE
# -------------------------------------------------------------------
{
    # IntyBASIC now accepts a library path for the
    # Prologue and Epilogue modules, so we don't
    # need to make symbolic links anymore! YAY!
    # check_lib($path);

    my $intybasic = $BAS_PATH . '/bin/intybasic';
    my $cmd_line  = "${ intybasic } ${ fullname } asm/${ asm_file } ${ LIB_PATH }";
    my $curr_dir = cwd();

    # Go to the source directory
    if ($path)
        {
        chdir "${ path }" or usage($!);
        }

        $result = `${ cmd_line }`;      # Compile..!

    # Return to whence we came
    if ($path)
        {
        chdir "${ curr_dir }" or usage($!);
        }
}

# -------------------------------------------------------------------
# ASSEMBLE ROM BINARY
# -------------------------------------------------------------------
{
    my $as1600    = $SDK_PATH . '/bin/as1600';
    my $cmd_line  = "${ as1600 } -o ${ bin_dir }/${ name } -l ${ bin_dir }/${ name }.ls -s ${ bin_dir }/${ name }.sym -j ${ bin_dir }/${ name }.map ${ asm_dir }/${ asm_file }";

    $result = `${ cmd_line }`;
}

print "Done.\n";
exit;

# -------------------------------------------------------------------
# SUBROUTINES
# -------------------------------------------------------------------

sub usage
{
    my $err = shift(@_);

    print "ERROR: ${ err }\n" if ($err);

    my ($prog) = ($0 =~ /.*\/(.+)/);
    die("Usage: $prog source.bas\n");
}

# O HAI! I HAS NOT NEEDED ANIMOAR!
# KTHXBYE NANOCHESS!
sub check_lib
{
    my $path = shift(@_);

    # Silly IntyBasic, it expects the prologue and epilogue
    # library files to be in the current working directory.
    # We get around this by making symbolic links to the
    # library files in the Basic source directory and then
    # CHDIR to it prior to compilation.

    # Ensure we have a symbolic link to the prologue
    unless (-e "${ path }${ PROLOGUE }")
        {
        my $result = `ln -s ${ LIB_PATH }/${ PROLOGUE } ${ path }${ PROLOGUE }`
        }

    # Ensure we have a symbolic link to the epilogue
    unless (-e "${ path }${ EPILOGUE }")
        {
        my $result = `ln -s ${ LIB_PATH }/${ EPILOGUE } ${ path }${ EPILOGUE }`
        }
}

sub check_dirs
{
    my ($asm, $bin) = @_;

    # Ensure we have an "asm" directory
    unless (-e $asm)
        {
        mkdir($asm) or usage($!);
        }

    # Ensure we have a "bin" directory
    unless (-e $bin)
        {
        mkdir($bin) or usage($!);
        }
}
