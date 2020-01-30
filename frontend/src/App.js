import React, { useState } from 'react';
import './styles/main.scss'

import Tour from 'reactour'

import Start from './views/Start.js'

function App() {
  const [open, setOpen] = useState(true);

  const steps = [
    {
      content: ({goTo}) => (
        <div>
          <h2>We don't collect any data</h2>
          <h3>We're also open source! <br/>
            <a href='https://github.com/epeery/quiz' target='_blank' rel='noopener noreferrer'>Contribute here</a>
          </h3>
        </div>
      ),
    },
  ]

  return (
    <>
      <Tour
        steps={steps}
        showNavigation={false}
        showButtons={false}
        rounded={8}
        isOpen={open}
        disableInteraction={true}
        onRequestClose={() => setOpen(false)}
        />

      <div className="App">
        <Start />
      </div>
    </>
  );
}

export default App;
