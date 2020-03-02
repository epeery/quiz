import { createMachine, assign } from 'xstate';

const canGoBack= (context, event) => {
  return context.questionNumber > 0;
};


const canGoForward = (context, event) => {
  return context.questionNumber < context.data.length - 1;
};

export const quizMachine = createMachine({
  id: 'quiz',
  initial: 'active',
  context: {
    questionNumber: 0,
//  Question {question :: Text, questionInfo :: Text, topic :: Text, id :: Topics}
//  data :: [Question] - Response from API
    data: [],
//  answers :: [(Topics, Float)] - User's responses to questions
    answers: {},
  },
  states: {
    active: {
      on: {
        BACK: [
          {
            target: 'active',
            cond: canGoBack,
            actions: 'decrementQuestion'
          },
          // {
          //   target: 'active',
          //   actions: 'notifyBack',
          // }
        ],
        SUBMIT: [
          {
            target: 'active',
            cond: canGoForward,
            actions: ['storeAnswer', 'incrementQuestion']
          },
          {
            target: 'results',
            actions: 'storeAnswer'
          }
        ],
        SKIP: [
          {
            target: 'active',
            cond: canGoForward,
            actions: 'incrementQuestion'
          },
          {
            target: 'results',
          }
        ],
        INFO: 'info',
      }
    },
    info: {
      on: {
        BACK: 'active'
      }
    },
    results: {
      entry: 'notifyResults',
      type: 'final'
    }
  }
},{
  actions: {
    incrementQuestion: assign({
      questionNumber: (context, event) => context.questionNumber + 1
    }),
    decrementQuestion: assign({
      questionNumber: (context, event) => context.questionNumber - 1
    }),
    storeAnswer: assign({
      answers: (context, event) => ({ ...context.answers, [context.data[context.questionNumber].id]: event.value })
    })
  }
});


export const encodeAnswers = results => Object.keys(results).map(x => [x, results[x]])
