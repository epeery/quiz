import React, { useEffect } from 'react';
import { useMachine } from '@xstate/react';
import { fetchMachine } from '../components/fetchMachine.js'

function Fetch({answers}) {
  const [current, send] = useMachine(fetchMachine, {
    services: {
      fetchData: (_, e) => fetch(`/api/results/${JSON.stringify(answers)}`).then(res => res.json())
    }
  });


  useEffect(() => {
    send('FETCH')
  }, []);


  switch (current.value) {
    case 'failure':
      return (
        <>
          <h1>Error while trying to get results</h1>
          <h3>Unable to connect to the server</h3>
          <button onClick={() => send('RETRY')} className='opinion-button' data-color='5'>
            <p>Retry</p>
          </button>
        </>
      );
    case 'success':
      const candidates = current.context.data.sort((a, b) => a < b ? 1 : -1)

      // const calcPercent = a => Math.round(a * 100)
      const calcPercent = num => Math.round(((num * 100) + 0.00001) * 10) / 10

      const winner = calcPercent(candidates[0][1]) !== calcPercent(candidates[1][1]) ? (
        <>
          <h1>We have a winner!</h1>
          <div className="candidate-pic-container large">
            <img className='candidate-pic large' src={candidates[0][0].rPic} alt={candidates[0][0].rName}/>
          </div>
          <h2>{candidates[0][0].rName}</h2>
          <h3>{calcPercent(candidates[0][1])}% match</h3>
        </>
      ) : (<h1>It's a tie!</h1>);
      return (
        <div className="candidates">
          <div className='winner'>
            {winner}
          </div>
          {candidates.map(([candidate, amount], i) => {
            const percent = calcPercent(amount);

            const color = percent > 70 ? 'blue1' :
                          percent > 50 ? 'blue2' :
                          percent > 40 ? 'pink1' :
                          /*otherwise*/  'pink2'

            return (
              <div className='candidate-container' key={i}>
                <div className="candidate-pic-container small">
                  <img className='candidate-pic small' src={candidate.rPic} alt={candidate.rName}/>
                </div>
                <div className='candidate-ranking'>
                  <div className='candidate-ranking-text'>
                    <h2>{candidate.rName}</h2>
                    <h2>{percent}%</h2>
                  </div>
                  <div className={'candidate-match-bar ' + color} style={{width: percent + '%', animationDelay: i * 0.05 + 's'}}></div>
                </div>
              </div>
            )
          })}
        </div>
      );
    default:
        return (
          <>
            <h1>Loading</h1>
          </>
        );

  }
}

function Results({answers}) {

  return (
    <div className='Results'>
      <Fetch answers={answers} onResolve={console.log}/>
    </div>
  )
}

export default Results;
