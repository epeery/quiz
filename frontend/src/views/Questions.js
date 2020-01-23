import React, { useState } from 'react';

import Tour from 'reactour'

import { useMachine } from '@xstate/react';
import { quizMachine, idToString, encodeAnswers } from '../components/quizMachine.js'

function Questions({data, onBack, onResults}) {
  const [current, send] = useMachine(quizMachine, {
    context: {
      data: data,
    },
    actions: {
      notifyBack: () => onBack(),
      notifyResults: ctx => onResults(encodeAnswers(ctx.answers))
    },
  });

  const [open, setOpen] = useState(true);

  const steps = [
    {
      content: ({goTo}) => (
        <div className='tutorial'>
          <h2>Welcome!</h2>
          <h3>Would you like a quick tutorial?</h3>
          <button onClick={() => goTo(1)} className='opinion-button' data-color='2'>
            <p>Ok!</p>
          </button>
        </div>
      ),
    },
    {
      content: ({goTo}) => (
        <div className='tutorial'>
          <h2>This is an opinion</h2>
          <h3>You'll be shown { current.context.data.length } of these in total</h3>
          <button onClick={() => goTo(2)} className='opinion-button' data-color='2'>
            <p>Next</p>
          </button>
        </div>
      ),
      selector: '.question'
    },
    {
      content: ({goTo}) => (
        <div className='tutorial'>
          <h2>These are stances</h2>
          <h3>You can select the one that best reflects how you feel about the opinion</h3>

          <button onClick={() => goTo(3)} className='opinion-button' data-color='2'>
            <p>Next</p>
          </button>
        </div>
      ),
      selector: '.opinion-container'
    },
    {
      content: ({goTo}) => (
        <div className='tutorial'>
          <h2>You can click the opinion for more information</h2>

          <button onClick={() => goTo(4)} className='opinion-button' data-color='2'>
            <p>Next</p>
          </button>
        </div>
      ),
      selector: '.question',
    },
    {
      content: ({goTo}) => (
        <div className='tutorial'>
          <h2>If you still aren't sure, you can always skip the question</h2>

          <button onClick={() => goTo(5)} className='opinion-button' data-color='2'>
            <p>Next</p>
          </button>
        </div>
      ),
      selector: '.skip',
    },
    {
      content: ({goTo}) => (
        <div className='tutorial'>
          <h2>You can also go back if you'd like to change a previous response</h2>

          <button onClick={() => goTo(6)} className='opinion-button' data-color='2'>
            <p>Next</p>
          </button>
        </div>
      ),
      selector: '.back',
    },
    {
      content: ({goTo}) => (
        <div className='tutorial'>
          <h2>That's it!</h2>
          <h3>You'll see a ranking of the democratic candidates once you're done</h3>

          <button onClick={() => setOpen(false)} className='opinion-button' data-color='2'>
            <p>Great!</p>
          </button>
        </div>
      ),
    },
  ]

  const questionNumber = current.context.questionNumber;

  const getSelected = () => (
    current.context.answers.hasOwnProperty(idToString(data[questionNumber].qId))
      ? current.context.answers[idToString(data[questionNumber].qId)]
      : undefined
  )

  const backClass = () => (questionNumber === 0 && current.value !== 'info') ? 'back dimmed' : 'back'

  return (
    <>
      <Tour
        steps={steps}
        showNavigation={false}
        showButtons={false}
        rounded={8}
        isOpen={open}
        disableInteraction={true}
        closeWithMask={false}
        onRequestClose={() => setOpen(false)} />

      <div className='progress-bar' style={{transform: 'translateX(' + -(100 - (100 * (questionNumber / current.context.data.length))) + '%)'}}></div>

      <div className='Questions'>
        <nav>
          <div onClick={() => send('BACK')} className={backClass()} >
            <div className="icon-container">
              <svg width='100%' height='100%' viewBox='0 0 14 12' fill='none' xmlns='http://www.w3.org/2000/svg'>
                <path d='M13 6H1M1 6L6.14286 1M1 6L6.14286 11' stroke='#31456A' strokeWidth='2' strokeLinecap='round' strokeLinejoin='round'/>
              </svg>
            </div>
            <p>Back</p>
          </div>

            {current.value === 'active' && (
              <div onClick={() => send('SKIP')} className='skip'>
                <p>{current.context.answers.hasOwnProperty(idToString(data[questionNumber].qId)) ? 'Next' : 'Skip' }</p>
                <div className="icon-container">
                  <svg width='100%' height='100%' viewBox='0 0 14 12' fill='none' xmlns='http://www.w3.org/2000/svg'>
                    <path d='M13 6H1M1 6L6.14286 1M1 6L6.14286 11' stroke='#31456A' strokeWidth='2' strokeLinecap='round' strokeLinejoin='round'/>
                  </svg>
                </div>
              </div>
            )}

        </nav>

        {current.value === 'active' && (
          <Question number={questionNumber + 1} topic={data[questionNumber].questionTopic} selected={getSelected()}>
            {data[questionNumber].question}
          </Question>
        )}
        {current.value === 'info' && (
          <Info question={data[questionNumber]}>
          </Info>
        )}

      </div>
    </>
  );


  function Question({children, number, topic, selected}) {
    const buttonClass = num => selected === num ? 'opinion-button selected' : 'opinion-button'
    return (
      <>
        <h3>Question {number} - {topic}</h3>
        <h2 className='question'><span onClick={() => send('INFO')}>{children}</span></h2>
        <div className='opinion-container'>
          <button onClick={() => send({type: 'SUBMIT', value: 1})} className={buttonClass(1)} data-color='1'>
            <p>Strongly Agree</p>
          </button>

          <button onClick={() => send({type: 'SUBMIT', value: 0.5})} className={buttonClass(0.5)} data-color='2'>
            <p>Agree</p>
          </button>

          <button onClick={() => send({type: 'SUBMIT', value: 0})} className={buttonClass(0)} data-color='3'>
            <p>Neutral</p>
          </button>

          <button onClick={() => send({type: 'SUBMIT', value: -0.5})} className={buttonClass(-0.5)} data-color='4'>
            <p>Disagree</p>
          </button>

          <button onClick={() => send({type: 'SUBMIT', value: -1})} className={buttonClass(-1)} data-color='5'>
            <p>Strongly Disagree</p>
          </button>
        </div>
      </>
    );
  }

}

function Info({question}) {
  return (
    <div className='info'>
      <h2 className='header'>{question.header}</h2>
      <p className='information'>“{question.info}”</p>
      <a className='source' href={question.source} target='_blank'><div>{question.source}</div></a>
    </div>
  );
}

export default Questions;
