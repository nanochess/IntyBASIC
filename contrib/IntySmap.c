/* ======================================================================== */
/*  Merge SMAP:  Simple utility for merging IntyBASIC 1.0 details into a    */
/*               source map file from AS1600.                               */
/*                                                                          */
/*  Author:  Joe Zbiciak.                                                   */
/*  Public domain.                                                          */
/*  Edited Jan/20/2017 by Oscar Toledo G. to correct crash with this line:  */
/*         while ( remap_sz[ asm_file ] < asm_lineno ) to <=                */
/*                                                                          */
/*  Note:  I'd prefer to write this in C++11, but I instead wrote it in C   */
/*  to ensure the greatest portability across compilers and platforms.      */
/* ======================================================================== */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifdef NEED_STRDUP
char *strdup( const char *s )
{
    char *ss = malloc( strlen( s ) + 1 );
    return strcpy( ss, s );
}
#endif

#ifdef _WIN32
#define PATH_SEP '\\'

int is_absolute_path( const char *const p )
{
    return p[0] == '\\' || p[0] == '/' ||
           (isalpha( p[0] ) && p[1] == ':' && ( p[2] == '\\' || p[2] == '/'));
}
#else
#define PATH_SEP '/'
int is_absolute_path( const char *const p )
{
    return p[0] == '/';
}
#endif

char **strmemo     = 0;
int    strmemo_sz  = 0;
int    strmemo_cnt = 0;

int memo_string( const char *str )
{
    int i;
    for ( i = 0; i < strmemo_cnt; i++ )
    {
        if ( !strcmp( str, strmemo[i] ) )
            return i;
    }

    if ( strmemo_cnt >= strmemo_sz )
    {
        strmemo_sz = strmemo_sz ? strmemo_sz << 1 : 16;
        strmemo = realloc( strmemo, strmemo_sz * sizeof( char * ) );
    }

    strmemo[ strmemo_cnt++ ] = strdup( str );
    return strmemo_cnt - 1;
}

char  *listing    = NULL;
char  *cwd        = NULL;
int    cwd_len    = 0;
char **path_array = NULL;
int    num_path   = 0;

void add_to_path( const char *p )
{
    path_array = realloc( path_array, ++num_path * sizeof(char *) );
    path_array[ num_path - 1 ] = strdup( p );
}

void set_cwd( const char *p )
{
    if ( cwd ) free( cwd );
    cwd = strdup( p );
    cwd_len = strlen( cwd );
}

void set_listing( const char *l )
{
    if ( listing ) free( listing );
    listing = strdup( l );
}


FILE *open_with_path( int file_memo )
{
    const char *const file = strmemo[ file_memo ];
    FILE *f = fopen( file, "r" );
    int cur_path = 0, file_len;

    if ( is_absolute_path( file ) )
    {
        if ( !f ) 
        {
            perror("fopen()");
            fprintf( stderr, "could not open file %s for reading", file );
            exit(1);
        }
    }

    file_len = strlen( file );

    while ( !f && cur_path < num_path )
    {
        int abs_path = is_absolute_path( path_array[ cur_path ] );
        int cur_len  = strlen( path_array[ cur_path ] );
        int to_alloc = cur_len + file_len + 2 + ( abs_path ? 0 : cwd_len + 1);

        char *filepath = malloc( to_alloc );

        if ( abs_path )
            sprintf( filepath, "%s%c%s", 
                     path_array[ cur_path ], PATH_SEP, file );
        else
            sprintf( filepath, "%s%c%s%c%s", 
                     cwd, PATH_SEP, path_array[ cur_path ], PATH_SEP, file );

        f = fopen( filepath, "r" );

        free( filepath );
        cur_path++;
    }

    if ( !f )
        fprintf( stderr, "Could not find file %s to open it.\n", file );

    return f;
}

#define BUFSZ ( 32768 )
char buf[ BUFSZ ];

typedef struct smap_rec_t smap_rec_t;

struct smap_rec_t
{
    smap_rec_t *next;
    int         src_file;   /* in memo_string table */
    int         s_addr;
    int         e_addr;
    int         flags;
    int         src_line;
    int         lst_line;
};

smap_rec_t *smap_head = NULL, *smap_tail = NULL;

int *asm_files;
int asm_file_cnt = 0;

void record_asm_file( int asm_file )
{
    int i;

    for ( i = 0; i < asm_file_cnt; i++ )
        if ( asm_files[ i ] == asm_file )
            return;

    asm_files = realloc( asm_files, ++asm_file_cnt * sizeof( int ) );

    asm_files[ asm_file_cnt - 1 ] = asm_file;
}

void slurp_smap( const char *smap_fname )
{
    int   cur_file = memo_string( "UNKNOWN" );
    FILE *f = fopen( smap_fname, "r" );

    if ( !f )
    {
        perror( "fopen()" );
        fprintf( stderr, "Could not open smap file %s\n", smap_fname );
        exit( 1 );
    }

    while ( fgets( buf, BUFSZ, f ) )
    {
        char *c;
        smap_rec_t *rec = calloc( 1, sizeof( smap_rec_t ) );

        if ( ( c = strchr( buf, '\r' ) ) != NULL ) *c = 0;
        if ( ( c = strchr( buf, '\n' ) ) != NULL ) *c = 0;

        if ( !strncmp( buf, "CWD ", 4 ) )
        {
            set_cwd( buf + 4 );
            continue;
        }

        if ( !strncmp( buf, "LISTING ", 8 ) )
        {
            set_listing( buf + 8 );
            continue;
        }

        if ( !strncmp( buf, "PATH ", 5 ) )
        {
            add_to_path( buf + 5 );
            continue;
        }
        
        if ( !strncmp( buf, "FILE ", 5 ) )
        {
            cur_file = memo_string( buf + 5 );
            record_asm_file( cur_file );
            continue;
        }

        if ( sscanf( buf, "%x %x %x %d %d", 
                    &( rec->s_addr ), &( rec->e_addr ), &( rec->flags ),
                    &( rec->src_line ), &( rec->lst_line ) ) != 5 )
        {
            free( rec );
            fprintf( stderr, "WARNING: unhandled smap line \"%s\"\n", buf );
            continue;
        }

        rec->src_file = cur_file;
        
        if ( !smap_head ) 
            smap_head = smap_tail = rec;
        else
        {
            smap_tail->next = rec;
            smap_tail       = rec;
        }
    }

    fclose( f );
}

typedef struct remap_rec_t
{
    int bas_file;
    int bas_lineno;
} remap_rec_t;

remap_rec_t **remap_tbl = NULL;
int          *remap_sz  = NULL;
int           remap_tbl_sz = 0;

void init_remap_tbl( void )
{
    /* this relies on the strmemo_tbl already having all the ASM file names */
    remap_tbl = calloc( strmemo_cnt, sizeof( remap_rec_t ) );
    remap_sz  = calloc( strmemo_cnt, sizeof( int         ) );

    remap_tbl_sz = strmemo_cnt;
}

void put_remap( int asm_file, int asm_lineno, int bas_file, int bas_lineno )
{
    /* Usu. iterates 0 or 1 times.  I don't care if it iterates 2+ times */
    while ( remap_sz[ asm_file ] <= asm_lineno )
    {
        int old_sz = remap_sz[ asm_file ], i;
        int new_sz = remap_sz[ asm_file ] = old_sz ? old_sz * 2 : 1024;

        remap_tbl[ asm_file ] = realloc
        ( 
            remap_tbl[ asm_file ], new_sz * sizeof( remap_rec_t )
        );

        for ( i = old_sz ; i < new_sz ; i++ )
        {
            remap_tbl[ asm_file ][ i ].bas_file   = -1;
            remap_tbl[ asm_file ][ i ].bas_lineno = -1;
        }
    }

    remap_tbl[ asm_file ][ asm_lineno ].bas_file   = bas_file;
    remap_tbl[ asm_file ][ asm_lineno ].bas_lineno = bas_lineno;
}

const remap_rec_t* get_remap( int asm_file, int asm_lineno )
{
    if ( remap_sz[ asm_file ] <= asm_lineno )
        return NULL;

    if ( remap_tbl[ asm_file ][ asm_lineno ].bas_file < 0 )
        return NULL;

    return &( remap_tbl[ asm_file ][ asm_lineno ] );
}


void process_asm( int asm_file )
{
    int bas_file   = -1;
    int asm_lineno = 0, bas_lineno = 0;
    FILE *f = open_with_path( asm_file );

    while ( fgets( buf, BUFSZ, f ) != NULL )
    {
        char *c;
        int l;

        asm_lineno++;

        if ( ( c = strchr( buf, '\r' ) ) != NULL ) *c = 0;
        if ( ( c = strchr( buf, '\n' ) ) != NULL ) *c = 0;

        if ( strstr( buf, "Epilogue for IntyBASIC programs" ) )
            break;

        if ( !strncmp( buf, "\t;FILE ", 7 ) )
        {
            bas_file = memo_string( buf + 7 );
            continue;
        }

        if ( sscanf( buf, "\t;[%d]", &l ) == 1 )
        {
            bas_lineno = l;
            continue;
        }

        if ( !strncmp( buf, "\t;ENDFILE", 9 ) )
        {
            bas_file = -1;
            continue;
        }
        if ( bas_file > 0 )
            put_remap( asm_file, asm_lineno, bas_file, bas_lineno );
    }

    fclose( f );

    return;
}
        
void filter_smap( void )
{
    smap_rec_t *smap_rec = smap_head;

    while ( smap_rec )
    {
        const remap_rec_t* remap_rec = get_remap( smap_rec->src_file, 
                                                  smap_rec->src_line );

        if ( remap_rec )
        {
            smap_rec->src_file = remap_rec->bas_file;
            smap_rec->src_line = remap_rec->bas_lineno;
            smap_rec->lst_line = 0;
        }
        smap_rec = smap_rec->next;
    }

    return;
}

void rewrite_smap( const char *smap_fname )
{
    char *rename_fname = malloc( strlen( smap_fname ) + 2 );
    FILE *f;
    int i, cur_file = -1;
    smap_rec_t *smap_rec;

    sprintf( rename_fname, "%s~", smap_fname ); /* I'm lazy. */
    
    rename( smap_fname, rename_fname );         /* a nicety */

    if ( !(f = fopen( smap_fname, "w" ) ) )
    {
        perror("fopen()");
        fprintf( stderr, "Could not open %s for writing\n", smap_fname ); 
        exit( 1 );
    }

    if ( cwd ) fprintf( f, "CWD %s\n", cwd );
    
    for ( i = 0; i < num_path; i++ )
        fprintf( f, "PATH %s\n", path_array[ i ] );

    if ( listing ) fprintf( f, "LISTING %s\n", listing );

    for ( smap_rec = smap_head ; smap_rec ; smap_rec = smap_rec->next )
    {
        if ( cur_file < 0 || cur_file != smap_rec->src_file )
        {
            cur_file = smap_rec->src_file;
            fprintf( f, "FILE %s\n", strmemo[ cur_file ] );
        }

        fprintf( f, "%04X %04X %02X %d %d\n", 
                 smap_rec->s_addr, smap_rec->e_addr, smap_rec->flags, 
                 smap_rec->src_line, smap_rec->lst_line );
    }

    fclose( f );
}

/*
** Main program
*/
int main( int argc, char *argv[] )
{
    int i;

    if ( argc != 2 )
    {
        fprintf( stderr, "Usage: %s foo.smap\n", argv[0] );
        exit( 1 );
    }

    slurp_smap( argv[1] );
    init_remap_tbl( );
    
    for ( i = 0 ; i < asm_file_cnt ; i++ )
    {
        const int asm_file = asm_files[ i ];
        printf( "processing %s...\n", strmemo[ asm_file ] );
        process_asm( asm_file );
    }

    filter_smap( );
    rewrite_smap( argv[1] );

    return 0;
}
