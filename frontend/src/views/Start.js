import React from 'react';
import { assign } from 'xstate';
import { useMachine } from '@xstate/react';
import { fetchMachine } from '../components/fetchMachine.js'
import { routingMachine } from '../components/routingMachine.js'

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
          <h1>Political Quiz</h1>
          <h3>Unable to connect to the server</h3>
          <button onClick={() => send('RETRY')} className='opinion-button' data-color='5'>
            <p>Retry</p>
          </button>
        </>
      );
    case 'loading':
      return (
        <>
          <h1>Political Quiz</h1>
          <button className='opinion-button' data-color='3'>
            <p>Loading...</p>
          </button>
        </>
      );
    default:
        return (
          <>
            <h1>Political Quiz</h1>
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
        <div className='Start'>
          <Fetch onResolve={data => send({type: 'START', data})} />
        </div>
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
