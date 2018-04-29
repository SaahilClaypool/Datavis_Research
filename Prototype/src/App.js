import React, { Component } from 'react';
import {Interactive} from './Interactive'; 
import './App.css';
import { Question } from './Question';
import { generate_data, getRandomInt } from './Util';
import {FrequencyChart} from './FrequencyChart'
import {TextVis} from './TextVis'
import {StaticVis} from './StaticVis'

class App extends Component {

  screen = 0;
  row = {}; 
  postId = (+new Date()).toString(36)

  constructor(props) {
    super(props); 
    this.state = {
      results: [],
      data:  generate_data()
    }
  }


  /**
   * @param guessA guess for testing positive
   * @param guessB guess for having disease
   * @param visType vis that was used to answer question
   * @param questionN number of people that were used in the question (default 100)
   */
  nextScreen = (guessA, guessB, data, visType, questionN, time) => {
    console.log("This should save the data for later");
    for(var i = 0; i < this.state.results.length; i++) {
      console.log(this.state.results[i])
    }
    let row = {
      guessA: guessA, guessB: guessB, data: data, visType: visType, questionN: questionN, time: time
    }
    this.row = row; 
    console.log(row);
    this.setState({
      results: [...this.state.results , row],
      data:  generate_data()
    });
    this.screen += 1; 
  }

  renderProblem = () => {
      let data = this.state.data;
      let vis = <div />;
      let visType = "";
      let visNum = getRandomInt(3);
      // let visNum = 0; 
      if (visNum == 0) {
        vis = <Interactive data={data} />
        visType = "interactive";
      } else if (visNum == 1) {
        vis = <FrequencyChart {...data} test={0} condition={0} />
        visType = "static_interactive";
      } else if (visNum === 2) {
        vis = <TextVis {...data} />
        visType = "text";
      } else if (visNum === 3) {
        vis = <StaticVis {...data} />
        visType = "static";
      }

      console.log("App is rendering");
      return (
        <div className="App">
          <header className="App-header">
            <h1>Bayesian inference</h1>
          </header>
          <div className="Content">
            <Question data={data} nextScreen={this.nextScreen} visType={visType}>
              {vis}
            </Question>
          </div>
        </div>
      );
  }

  renderQuestionnaire = () =>  {
    let form = (
      <div>
          <form 
              onSubmit={(e) => { 
                  e.preventDefault();
                  this.parseData(); 
              }}
          >
              <label>gender:
                  <select id="gender" name="gender" required={true}>
                      <option value="Other">Other</option>
                      <option value="Male">Male</option>
                      <option value="Female">Female</option>
                  </select>
              </label>
              <br/>

              <label>age: 
                  <input type="number" min="0" max="100" id="age" placeholder={"Your age"} required={true}/>
              </label>
              <br />
              <label>Experience with Statistics:
                  <select id="experience" name="experience" required={true}>
                      <option value="High">High</option>
                      <option value="Medium">Medium</option>
                      <option value="Low">Low</option>
                  </select>
              </label>
              <br/>
              <label>Education:
                  <select id="education" name="education" required={true}>
                      <option value="High School">High School</option>
                      <option value="2 Year College">2 year College</option>
                      <option value="4 Year College">4 Year College</option>
                      <option value="Masters">Masters</option>
                      <option value="PhD">PhD</option>
                      <option value="MD">MD</option>
                  </select>
              </label>
              <br/>
              <h4>Note: Hitting Send will send your results to the proctor</h4>
              <input type="submit" value="Send Data to Proctor" />
          </form>
      </div>
  );

  let basic_result = <div/>;
  if(this.state.row) {
    basic_result = (
      <div>
          <p>Thank you!</p>
          <p>Use the survey code to verify the completion of your task</p>
          <p>code: {this.postId}</p>
      </div>
      ); 
  }
  return (
      <div className="Survey">
          <h1>Please enter a bit about yourself</h1>
          {form}
          {basic_result}
      </div>
  );
  }

  parseData = () => {
    let gender = document.getElementById("gender");
    let genderVal = gender.value; 
    let age = document.getElementById("age");
    let ageVal = parseInt(age.value, undefined);
    if (isNaN(ageVal)) {
        ageVal = 0;
    }
    let experience = document.getElementById("experience");
    let expVal = experience.value; 
    let education = document.getElementById("education");
    let edValue = education.value; 
    console.log("Post id: " + this.postId)
    this.row = {...this.row, age: ageVal, gender: genderVal, experience: expVal, education: edValue, postId: this.postId};
    // window.location.href = "mailto:smclaypool@wpi.edu?subject=Experiment%20Results&body=" + JSON.stringify(this.row);
    this.setState({
      ...this.state, 
        row: JSON.stringify(this.row)
    });
    console.log(this.row)
    fetch('/data', {
    method: 'post',
    mode: 'no-cors',
    body: JSON.stringify(this.row)});

}

  render() {
    if (this.screen === 0) {
      return this.renderProblem(); 
    }
    else {
      return this.renderQuestionnaire(); 
    }
  }
}

export default App;
