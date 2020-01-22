import { createMachine } from 'xstate';

export const routingMachine = createMachine({
  id: 'routing',
  initial: 'start',
  states: {
    start: {
      on: { START: 'quiz' }
    },
    quiz: {
      entry: 'quizEntered',
      on: {
        BACK: 'start',
        FINISHED: 'results'
      }
    },
    results: {
      entry: 'resultsEntered',
      type: 'final'
    }
  }
});
