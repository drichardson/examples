//
//  main.m
//  HIDGamePad
//
//  Created by Doug on 4/30/11.
//  Copyright 2011 Doug Richardson. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <SDL/SDL.h>

#undef main

int main (int argc, char * argv[])
{
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    
    if ( SDL_Init(SDL_INIT_EVERYTHING /*SDL_INIT_JOYSTICK*/) )
    {
        puts("Error initializing SDL");
        return 1;
    }
    
    int numJoysticks = SDL_NumJoysticks();
    
    if ( numJoysticks == 0 )
    {
        puts("No joysticks.");
        return 1;
    }
    
    printf("I have %d joysticks\n", numJoysticks);
    
    for(int i = 0; i < numJoysticks; ++i)
    {
        printf("Joystick %d is named %s\n", i, SDL_JoystickName(i));
    }
    
    printf("Going to use joystick at index 0: %s\n", SDL_JoystickName(0));
    
    SDL_Joystick* joystick = SDL_JoystickOpen(0);
    
    int numAxes = SDL_JoystickNumAxes(joystick);
    
    printf("axis: %d, buttons: %d, balls: %d, hats: %d\n", numAxes, SDL_JoystickNumBalls(joystick), SDL_JoystickNumBalls(joystick), SDL_JoystickNumHats(joystick));
    
    dispatch_source_t timer = dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER, 0, 0, dispatch_get_main_queue());
    int timesPerSecond = NSEC_PER_SEC / 60;
    dispatch_source_set_timer(timer, DISPATCH_TIME_NOW, timesPerSecond, timesPerSecond / 2);
    
    dispatch_source_set_event_handler(timer, ^{
        
        const int kFilterValues = 2500;
        SDL_Event event;
        
        while ( SDL_PollEvent(&event) )
        {
            switch(event.type)
            {
                case SDL_JOYBUTTONDOWN:
                case SDL_JOYBUTTONUP:
                    printf("Joystick %d button %d %s\n", event.jbutton.which, event.jbutton.button, event.jbutton.state == SDL_PRESSED ? "DOWN" : "UP");
                    break;
                    
                case SDL_JOYAXISMOTION:
                    if ( event.jaxis.value > kFilterValues || event.jaxis.value < -kFilterValues )
                    {
                        printf("Joystick %d axis %d motion %d\n", event.jaxis.which, event.jaxis.axis, event.jaxis.value);
                    }
                    break;
                    
                default:
                    printf("Other event: %d\n", event.type);
                    break;
            }
        }
    });
    
    dispatch_resume(timer);
    
    dispatch_main();
    
    dispatch_release(timer);
    
    SDL_JoystickClose(joystick);
    
    [pool drain];
    return 0;
}

