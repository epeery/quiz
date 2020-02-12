import React, { useState } from 'react';
import './styles/main.scss'

import Start from './views/Start.js'

function App() {
  return (
    <div className="App">
      <a className="logo" href="https://insight.vote"><h2>insight<span>.</span>vote</h2></a>

      <Start />

    </div>
  );
}

export default App;
