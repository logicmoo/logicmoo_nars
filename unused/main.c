/* 
 * The MIT License
 *
 * Copyright 2020 The OpenNARS authors.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include <time.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "NAR.h"
#include "./unit_tests/unit_tests.h"
#include "./system_tests/system_tests.h"
#include "Shell.h"
#include "./NetworkNAR/UDPNAR.h"

void Process_Args(int argc, char *argv[])
{
    bool inspectionOnExit = false;
    long iterations = -1;
    if(argc >= 4)
    {
        if(!strcmp(argv[3],"InspectionOnExit"))
        {
            inspectionOnExit = true;
        }
    }
    if(argc >= 3)
    {
        if(!strcmp(argv[2],"InspectionOnExit"))
        {
            inspectionOnExit = true;
        }
        else
        {
            iterations = atol(argv[2]);
        }
    }
    if(argc >= 2)
    {
        if(!strcmp(argv[1],"NAL_GenerateRuleTable"))
        {
            NAR_INIT();
            NAL_GenerateRuleTable();
            exit(0);
        }
        if(!strcmp(argv[1],"pong"))
        {
            NAR_Pong(iterations);
        }
        if(!strcmp(argv[1],"pong2"))
        {
            NAR_Pong2(iterations);
        }
        if(!strcmp(argv[1],"testchamber"))
        {
            NAR_TestChamber();
        }
        if(!strcmp(argv[1],"alien"))
        {
            NAR_Alien(iterations);
        }
        if(!strcmp(argv[1],"cartpole"))
        {
            NAR_Cartpole(iterations);
        }
        if(!strcmp(argv[1],"robot"))
        {
            NAR_Robot(iterations);
        }
        if(!strcmp(argv[1],"shell"))
        {
            Shell_Start();
        }
        if(!strcmp(argv[1],"UDPNAR")) // ./NAR UDPNAR IP PORT timestep(ns per cycle) printDerivations
        {
            char *ip = argv[2];
            int port = atoi(argv[3]);
            long timestep = atol(argv[4]);
            bool printDerivations = !strcmp("true", argv[5]);
            PRINT_DERIVATIONS = printDerivations;
            UDPNAR_Start(ip, port, timestep);
            puts("//press any key and enter to quit!");
            fflush(stdout);
            getchar();
            UDPNAR_Stop();
        }
    }
    if(inspectionOnExit)
    {
        puts("*concepts");
        Shell_ProcessInput("*concepts");
        puts("*done");
        puts("*cycling_belief_events");
        Shell_ProcessInput("*cycling_belief_events");
        puts("*done");
        puts("*cycling_goal_events");
        Shell_ProcessInput("*cycling_goal_events");
        puts("*done");
        puts("*stats");
        Shell_ProcessInput("*stats");
        puts("*done");
    }
}

void Display_Help()
{
    puts("\nAll tests ran successfully, if you wish to run examples now, just pass the corresponding parameter:");
    puts("NAR pong (starts Pong example)");
    puts("NAR pong2 (starts Pong2 example)");
    puts("NAR testchamber (starts Test Chamber multistep procedure learning example)");
    puts("NAR alien (starts the alien example)");
    puts("NAR cartpole (starts the cartpole example)");
    puts("NAR robot (starts the robot example)");
    puts("NAR shell (starts the interactive NAL shell)");
}

int main(int argc, char *argv[])
{
    mysrand(1337);
    Process_Args(argc, argv);
    if(argc == 1)
    {
        NAR_INIT();
        Run_Unit_Tests();
        Run_System_Tests();
        Display_Help();
    }
    return 0;
}

