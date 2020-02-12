import React from 'react';
import { assign } from 'xstate';
import { useMachine } from '@xstate/react';

import { fetchMachine } from '../components/fetchMachine.js'
import { routingMachine } from '../components/routingMachine.js'
import Banner from '../components/Banner.js'

import Questions from './Questions.js'
import Results from './Results.js'

function Fetch({onResolve, onResults}) {
  const [current, send] = useMachine(fetchMachine, {
    actions: {
      notifySuccess: ctx => onResolve(ctx.data)
    },
    services: {
      fetchData: (_, e) => fetch(`${process.env.REACT_APP_API_URL}/api/questions`).then(res => res.json())
    }
  });


  switch (current.value) {
    case 'failure':
      return (
        <>
          <h3>Unable to connect to the server</h3>
          <button onClick={() => send('RETRY')} className='opinion-button' data-color='5'>
            <p>Retry</p>
          </button>
        </>
      );
    case 'loading':
      return (
        <>
          <button className='opinion-button' data-color='3'>
            <p>Loading...</p>
          </button>
        </>
      );
    default:
        return (
          <>
            <button onClick={() => send('FETCH')} className='opinion-button' data-color='1'>
              <p>Start</p>
            </button>
          </>
        );

  }
}

function Start() {

  const [current, send] = useMachine(routingMachine, {
    actions: {
      quizEntered: assign({data: (_, e) => e.data}),
      resultsEntered: assign({answers: (_, e) => e.answers})
    }
  });

  switch (current.value) {
    default:
      return (
        <>
          <div className='Start'>
            <h1>Find your political match</h1>
            <Fetch onResolve={data => send({type: 'START', data})} />

            <Banner>
              <h2>We don't collect any data</h2>
              <h3>We're also open source! <br/>
                <a href='https://github.com/epeery/quiz' target='_blank' rel='noopener noreferrer'>Contribute here</a>
              </h3>
            </Banner>
          </div>
        </>
      )
    case 'quiz':
      return (
        <Questions data={current.context.data} onBack={() => send('BACK')} onResults={answers => send({type: 'FINISHED', answers})}></Questions>
      )
    case 'results':
      return (
        <Results answers={current.context.answers} />
      )
  }
}

export default Start;
