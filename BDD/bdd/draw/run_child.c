/*
 DOCUMENTATION INFORMATION				          module: PLOT
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : run_child.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1991-1994 G.L.J.M. Janssen
 date	   :  7-JUN-1994
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */
#include <stdio.h>
#include <signal.h>
#include <assert.h>
#include <errno.h>
#include <string.h>

#include "alloc.h"
#include "run_child.h"

/* ------------------------------------------------------------------------ */
/* LOCAL DEFINES                                                            */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS                                                   */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL VARIABLES                                                          */
/* ------------------------------------------------------------------------ */
static char SccsId[] = "%Z%%Y%/%M% %I% %G%";

static pid_t pid = 0;		/* child's process id            */
static int in_fd[2], out_fd[2];	/* child's pipe file descriptors */

/* ------------------------------------------------------------------------ */
/* IMPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL FUNCTION PROTOTYPES                                                */
/* ------------------------------------------------------------------------ */

/* ************************************************************************ */
/* FUNCTION DOCUMENTATION:                                                  */
/*                                                                          */
/* ************************************************************************ */

/* SIGINT handler. */
static void sigint_handler (int arg)
{
  print_message ("IRCP009", "Received SIGINT signal.");
  print_message ("IRCP010", "Waiting for any child processes to die.");
  wait (0);
  print_message ("IRCP011", "Terminating execution.");
  exit (1);
}

/* SIGPIPE handler. */
static void sigpipe_handler (int arg)
{
  print_message ("IRCP013", "Received SIGPIPE signal.");
  print_message ("IRCP014", "Waiting for any child processes to die.");
  wait (0);
  print_message ("IRCP015", "Resuming parent process execution.");
  /* Reset signal handler: */
  signal (SIGPIPE, sigpipe_handler);
}

/* Child exit handler. Closes pipes. */
static void sigchld_handler (int arg)
{
  if (!pid)
    return;

  if (pid != wait (0)) {
    /* Reset signal handler: */
    signal (SIGCHLD, sigchld_handler);
    return;
  }

  /* Mark process terminated: */
  pid = 0;

  /* Close child's side of the pipes: */
  close (in_fd[0]);
  close (out_fd[1]);
}

/* Exit handler: Kill child process on program exit. */
static void kill_child (void)
{
  /* Ignore child termination signals: */
  signal (SIGCHLD, SIG_IGN);
  if (pid)
    kill (pid, SIGKILL);
}

/* Start child process.
   Assign its (standard) input and output to the file pointers
   `std_in' and `std_out'.
   `child_name' is name of program to execute as forked process,
   `options' is string of command-line options to be passed on,
   `Monitor' true means save data passed through pipes in files,
   `mon_file_name_prefix' is monitor files name prefix
   (defaults to child_name),
   As a side-effect temporarily sets up exit and signals handlers.
   Returns 1 upon success, else 0.
   (An errors are reported to stderr).
*/
int run_child (const char *child_name,
	       const char *options,
	       int Monitor,
	       const char *mon_file_name_prefix,
	       FILE **std_in, FILE **std_out)
{
  static int first_call = 1;
  char cmd_line[BUFSIZ];

  /* Create pipes: */
  if (pipe (in_fd)) {
    print_message ("ERCP001",
		   "[run_child]: errno %d: %s", errno, strerror (errno));
    return 0;
  }

  if (pipe (out_fd)) {
    print_message ("ERCP002",
		   "[run_child]: errno %d: %s", errno, strerror (errno));
    return 0;
  }

  /* Install exit handlers: */
  if (first_call) {
    /* Set up routine for cleanup processing at exit: */
    atexit (kill_child);
    first_call = 0;
  }

  /* Set-up signal handlers: */
  signal (SIGINT,  sigint_handler);
  signal (SIGCHLD, sigchld_handler);
  signal (SIGPIPE, sigpipe_handler);

  if (Monitor) {
    char iname[BUFSIZ], oname[BUFSIZ];

    sprintf (iname, "%s.in",
	     mon_file_name_prefix ? mon_file_name_prefix : child_name);
    sprintf (oname, "%s.out",
	     mon_file_name_prefix ? mon_file_name_prefix : child_name);
    sprintf (cmd_line, "tee %s | %s %s | tee %s",
	     iname, child_name, options, oname);
    print_message ("IRCP008",
		   "%s input in file '%s', its output in file '%s'.\n",
		   child_name, iname, oname);
  }

  if ((pid = fork()) == (pid_t) 0) {
    /* Child process. */

    /* Reassign stdin: */
    if (close (0) || dup (in_fd[0]) != 0) {
      print_message ("ERCP004",
        "[run_child]: errno %d: %s", errno, strerror (errno));
      return 0;
    }
    close (in_fd[1]);

    /* Reassign stdout: */
    if (close (1) || dup (out_fd[1]) != 1) {
      print_message ("ERCP005",
        "[run_child]: errno %d: %s", errno, strerror (errno));
      return 0;
    }
    close (out_fd[0]);

    /* Start child: */
    if (Monitor)
      execlp ("sh", "sh", "-c", cmd_line, 0);
    else
      execlp (child_name, child_name, options, 0);

    /* Should never come here! */
    print_message ("ERCP006",
		   "[run_child]: errno %d: %s", errno, strerror (errno));
    return 0;
  }

  if (pid > (pid_t) 0) {
    /* This is the regular process. */

    /* Set file pointers for our side of the pipes: */
    *std_in  = fdopen ( in_fd[1], "w");
    *std_out = fdopen (out_fd[0], "r");

    close (in_fd[0]);
    close (out_fd[1]);

    if (!*std_in || !*std_out) {
      print_message ("ERCP007",
	     "[run_child]: errno %d: %s", errno, strerror (errno));
      kill (pid, SIGKILL);
      return 0;
    }
    return 1;
  }

  /* pid == 0 */
  print_message ("ERCP003",
		 "[run_child]: errno %d: %s", errno, strerror (errno));
  return 0;
}
