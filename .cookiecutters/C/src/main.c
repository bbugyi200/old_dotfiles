#include "{{ PROJECT }}.h"

#include <stdbool.h>
#include <getopt.h>

bool debug_flag;


int main(int argc, char **argv)
{
    char const *usage = "usage: {{ PROJECT }}";

    while (true)
    {
        static struct option long_options[] =
        {
            /* Argument styles: no_argument, required_argument, optional_argument */
            {"debug", no_argument, 0, 'D'},
            {"help", no_argument, 0, 'h'},
            {0,0,0,0}
        };
    
        int option_index = 0;
    
        /* --- Argument parameters ---
            no_argument: " "
            required_argument: ":"
            optional_argument: "::" */
        int choice = getopt_long(argc, argv, "dh",
                        long_options, &option_index);
    
        if (choice == -1)
            break;
    
        switch(choice)
        {
            case 'd':
                debug_flag = true;
                break;
            case 'h':
                printf("%s\n", usage);
                exit(0);
                break;
            case '?':
                /* getopt_long will have already printed an error */
                break;
            default:
                /* Not sure how to get here... */
                return EXIT_FAILURE;
        }
    }

    {% INSERT %}
}
