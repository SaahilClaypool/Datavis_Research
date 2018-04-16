/**
* This file will contain utility functions related to generating scenerios
* Data will be generated as follows: 
* - 100 people, 
* - preset set of 5 conditions randomly chosen
*/

/**
 * Array of possible configurations
 */
let data = [
    {
      has_condition: 0.1,
      positive_condition: 0.9,
      positive_no_condition: 0.1,
      number_of_people: 100,
    },
    {
      has_condition: 0.2,
      positive_condition: 0.7,
      positive_no_condition: 0.1,
      number_of_people: 100,
    },
    {
      has_condition: 0.25,
      positive_condition: 0.9,
      positive_no_condition: 0.5,
      number_of_people: 100,
    },
    {
      has_condition: 0.1,
      positive_condition: 0.9,
      positive_no_condition: 0.3,
      number_of_people: 100,
    },
    {
      has_condition: 0.2,
      positive_condition: 0.7,
      positive_no_condition: 0.4,
      number_of_people: 100,
    },
]

let previous = 0; 

export function generate_data() {
  let randIndex = Math.floor(Math.random() * data.length);
  while (randIndex === previous) {
    randIndex = Math.floor(Math.random() * data.length);
  }
  var rand = data[randIndex];
  previous = randIndex; 
  return rand;
}
export function getRandomInt(max) {
  return Math.floor(Math.random() * Math.floor(max));
}
