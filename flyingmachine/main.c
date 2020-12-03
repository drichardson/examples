#include <inttypes.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <avr/interrupt.h>

#define	output		0xFF

#define	SPEED_A	10
#define SPEED_B 50
#define	SPEED_C 200
#define PRSC1_SELECT 1


enum motorIdent { A=0x01, B=0x02, C=0x04, D=0x08 };
enum motorState { ON=1, OFF=0 };

void timer_init(void);
void motorSet(enum motorIdent motor, enum motorState state);

enum motorState dState;
uint8_t counter_a;
uint8_t counter_b;
uint8_t counter_c;

uint8_t speed_a;
uint8_t speed_b;
uint8_t speed_c;

int main(void)
{
	
	DDRD = 0xFF;
	PORTD = 0x00;

	dState = OFF;
	counter_a = 255;
	counter_b = 255;
	counter_c = 255;

	speed_a = SPEED_A;
	speed_b = SPEED_B;
	speed_c = SPEED_C;

	timer_init();
	sei();

	for(;;){}

	return 0;
}

ISR(TIMER0_OVF_vect){
	TCNT0 = 0;
	if(dState == OFF){
		//motorSet(D, ON);
		//dState = ON;
	}else{
		motorSet(D, OFF);
		dState = OFF;
	}
	
	speed_a += 5;
	speed_b += 5;
	speed_c += 5;
}

ISR(TIMER1_COMPA_vect)
{
  TCNT1 = 0;                     /* reset TCNT1 */
                                 
  //TCCR1B = PRSC1_SELECT;         /* set count rate */

		if(SPEED_A && (counter_a == 255)){
			counter_a = 0;
			motorSet( A, ON );
		}

		if(SPEED_B && (counter_b == 255)){
			counter_b = 0;		
			motorSet( B, ON);
		}

		if(SPEED_C && (counter_c == 255)){
			counter_c = 0;
			motorSet( C, ON);
		}

		if(counter_a >= speed_a)
			motorSet( A, OFF);

		if(counter_b >= speed_b)
			motorSet( B, OFF);
		
		if(counter_c >= speed_c)
			motorSet( C, OFF);
		

		counter_a++;
		counter_b++;
		counter_c++;
}

/* function */
void timer_init(void)
{
  TIMSK = (1 << OCIE1A) | (1 << TOIE0);            /* enable timer compare interrupt */
  TCCR1A = 0;                    /* disable PWM */
  TCCR1B = PRSC1_SELECT;         /* set count rate */ 
  TCCR0 = (1<<CS02)|(1<<CS00);
  
  TCNT1 = 0;                     /* reset TCNT1 */
  TCNT0 = 0;

  OCR1A = 80;
}


void motorSet(enum motorIdent motor, enum motorState state){

  if(state == ON)
    PORTD |= motor;
  else
    PORTD &= ~(motor);
}
