//
//  ViewController.m
//  Animations
//
//  Created by Douglas Richardson on 4/7/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import "ViewController.h"
#import <QuartzCore/QuartzCore.h>

@interface ViewController ()

@end

@implementation ViewController
{
    UIView* _explicit;
    UIView* _oldAnimationBlock;
    UIView* _uiAnimationBlock;
    UIView* _action;
    CALayer* _layerToSetActionsOn;
}

- (void)loadView
{    
    UIView* view = [[UIView alloc] initWithFrame:[[UIScreen mainScreen] applicationFrame]];
    view.backgroundColor = [UIColor greenColor];
    
    _explicit = [[UIView alloc] initWithFrame:CGRectMake(CGRectGetMinX(view.bounds), CGRectGetMinY(view.bounds), CGRectGetMidX(view.bounds), CGRectGetMidY(view.bounds))];
    _explicit.backgroundColor = [UIColor yellowColor];
    UITapGestureRecognizer* tapRecognizer = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(explicitTapped:)];
    tapRecognizer.numberOfTapsRequired = 1;
    [_explicit addGestureRecognizer:tapRecognizer];
    
    _oldAnimationBlock = [[UIView alloc] initWithFrame:CGRectMake(CGRectGetMaxX(_explicit.frame), 0, _explicit.bounds.size.width, _explicit.bounds.size.height)];
    _oldAnimationBlock.backgroundColor = [UIColor magentaColor];
    tapRecognizer = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(oldAnimationBlockTapped:)];
    tapRecognizer.numberOfTapsRequired = 1;
    [_oldAnimationBlock addGestureRecognizer:tapRecognizer];
    
    _uiAnimationBlock = [[UIView alloc] initWithFrame:CGRectMake(CGRectGetMinX(view.bounds), CGRectGetMidY(view.bounds), _explicit.bounds.size.width, _explicit.bounds.size.height)];
    _uiAnimationBlock.backgroundColor = [UIColor purpleColor];
    tapRecognizer = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(uiAnimationBlockTapped:)];
    tapRecognizer.numberOfTapsRequired = 1;
    [_uiAnimationBlock addGestureRecognizer:tapRecognizer];
    
    // Set actions on the sub-layer because setting the actions dictionary on the UIView's layer has
    // no effect because the UIView is the view.layer's delegate and overrides actionForLayer:forKey: so
    // that the layer's actions dictionary isn't searched.
    _action = [[UIView alloc] initWithFrame:CGRectMake(_oldAnimationBlock.frame.origin.x, _uiAnimationBlock.frame.origin.y, _explicit.bounds.size.width, _explicit.bounds.size.height)];
    _action.backgroundColor = [UIColor cyanColor];
    tapRecognizer = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(actionTapped:)];
    tapRecognizer.numberOfTapsRequired = 1;
    [_action addGestureRecognizer:tapRecognizer];
    CABasicAnimation* action = [CABasicAnimation animationWithKeyPath:@"transform"];
    action.duration = 1.0;
    _layerToSetActionsOn = [CALayer layer];
    _layerToSetActionsOn.actions = [NSDictionary dictionaryWithObjectsAndKeys:action, @"transform", nil];
    _layerToSetActionsOn.frame = _action.bounds;
    _layerToSetActionsOn.backgroundColor = [[UIColor blueColor] CGColor];
    [_action.layer addSublayer:_layerToSetActionsOn];
    
    [view addSubview:_explicit];
    [view addSubview:_oldAnimationBlock];
    [view addSubview:_uiAnimationBlock];
    [view addSubview:_action];
    
    self.view = view;
}

- (void)explicitTapped:(id)sender
{    
    if ( [_explicit.layer.animationKeys containsObject:@"transformAnimation"] )
    {
        [_explicit.layer removeAnimationForKey:@"transformAnimation"];
    }
    else
    {
        CABasicAnimation* animation = [CABasicAnimation animationWithKeyPath:@"transform"];
        animation.duration = 0.25;
        animation.toValue = [NSValue valueWithCATransform3D:CATransform3DMakeScale(0.5, 0.5, 1)];
        animation.fillMode = kCAFillModeForwards;
        animation.removedOnCompletion = NO;
        [_explicit.layer addAnimation:animation forKey:@"transformAnimation"];
    }    
}

- (void)oldAnimationBlockTapped:(id)sender
{
    [UIView beginAnimations:@"yourMomsAnimation" context:NULL];
    
    if ( CGAffineTransformIsIdentity(_oldAnimationBlock.transform) )
    {
        _oldAnimationBlock.transform = CGAffineTransformMakeTranslation(40, 40);
    }
    else
    {
        _oldAnimationBlock.transform = CGAffineTransformIdentity;
    }
    
    [UIView commitAnimations];
}

- (void)uiAnimationBlockTapped:(id)sender
{
    [UIView animateWithDuration:0.25 animations:^{
        if ( CGAffineTransformIsIdentity(_uiAnimationBlock.transform) )
        {
            _uiAnimationBlock.transform = CGAffineTransformConcat(CGAffineTransformMakeScale(0.25, 0.25), CGAffineTransformMakeRotation(M_PI_4));
        }
        else
        {
            _uiAnimationBlock.transform = CGAffineTransformIdentity;
        }
    }];
}

- (void)actionTapped:(id)sender
{    
    if ( CATransform3DIsIdentity(_layerToSetActionsOn.transform) )
    {
        _layerToSetActionsOn.transform = CATransform3DMakeRotation(M_PI_2, 0, 0, 1);
    }
    else
    {
        _layerToSetActionsOn.transform = CATransform3DIdentity;
    }
}

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view, typically from a nib.
}

- (void)viewDidUnload
{
    [super viewDidUnload];
    // Release any retained subviews of the main view.
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
}

@end
