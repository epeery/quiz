import React from 'react';

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

  const questionNumber = current.context.questionNumber;

  const getSelected = () => (
    current.context.answers.hasOwnProperty(idToString(data[questionNumber].id))
      ? current.context.answers[idToString(data[questionNumber].id)]
      : undefined
  )

  const backClass = () => (questionNumber === 0 && current.value !== 'info') ? 'back dimmed' : 'back'

  return (
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
              <p>{current.context.answers.hasOwnProperty(idToString(data[questionNumber].id)) ? 'Next' : 'Skip' }</p>
              <div className="icon-container">
                <svg width='100%' height='100%' viewBox='0 0 14 12' fill='none' xmlns='http://www.w3.org/2000/svg'>
                  <path d='M13 6H1M1 6L6.14286 1M1 6L6.14286 11' stroke='#31456A' strokeWidth='2' strokeLinecap='round' strokeLinejoin='round'/>
                </svg>
              </div>
            </div>
          )}

      </nav>

      {current.value === 'active' && (
        <Question number={questionNumber + 1} topic={data[questionNumber].topic} selected={getSelected()}>
          {data[questionNumber].question}
        </Question>
      )}
      {current.value === 'info' && (
        <Info info={data[questionNumber].questionInfo}>
        </Info>
      )}

    </div>
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

  function Info({info}) {
    console.log(info);
    return (
      <div className='info'>
        <h2 className='header'>{info.header}</h2>
        <p className='information'>“{info.info}”</p>
        <a className='source' href={info.source} target='_blank'><div>{info.source}</div></a>
      </div>
    );
  }
}

export default Questions;
