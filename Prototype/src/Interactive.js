import React, { Component } from 'react';
import { FrequencyChart } from './FrequencyChart'; 
import './Interactive.css'

export class Interactive extends Component {
  constructor(props) {
    super(props); 
    this.state = {
      condition: 0, 
      test: 0,
      p_test: 0, 
      p_condition: 0, 
      s_test:0, 
      s_condition: 0, 
    }
  }

  componentDidMount() {
    // animate
  }

  handle_mouse_enter_condition = (event) => {
    let val = parseInt(event.target.firstChild.value); 
    this.setState((prevState) => {
      return {
        test: this.state.test, condition: val,
        p_condition: this.state.condition,
        p_test: this.state.test
      };
    });
  }

  handle_mouse_leave_condition = (event) => {
    let val = parseInt(event.target.firstChild.value); 
    this.setState((prevState) => {
      return {
        test: this.state.test, condition: this.state.s_condition,
        p_condition: this.state.condition,
        p_test: this.state.test, 
      };
    });
  }

  handle_mouse_enter_test = (event) => {
    let val = parseInt(event.target.firstChild.value); 
    this.setState((prevState) => {
      return {
        test: val, condition: this.state.condition,
        p_condition: this.state.condition,
        p_test: this.state.test
      };
    });
  }

  handle_mouse_leave_test = (event) => {
    let val = parseInt(event.target.firstChild.value); 
    this.setState((prevState) => {
      return {
        test: this.state.s_test, condition: this.state.condition,
        p_condition: this.state.condition,
        p_test: this.state.test, 
      };
    });
  }

  handle_test_change = (event) => {
    let val = parseInt(event.target.value);
    this.setState((prevState) => {
      return {
        test: val, condition: prevState["condition"], 
        p_condition: prevState["condition"], 
        p_test: prevState["test"],
        s_condition: prevState["condition"], 
        s_test: prevState["test"],
      };
    });
  }

  handle_condition_change = (event) => {
    let val = parseInt(event.target.value);
    this.setState((prevState) => {
      return {
        condition: val, test: prevState["test"],
        p_condition: prevState["condition"], 
        p_test: prevState["test"],
        s_condition: prevState["condition"], 
        s_test: prevState["test"],
      }
    });
  }
  
  render() {
    let data = this.props.data;
    let condCheck = (val) => {
      if (this.state.condition === val) {
        return "checked"; 
      }
      return "false"; 
    }
    return (
      <div className="Interactive">
        <h3>What do you know?</h3>
        <br/>
        <p>Try hovering or clicking on the codition and test values to interact with the visualization</p>
        <p>Click on a value to set that value. Mouse over a value to preview the visualization with that value.</p>
        <div className="Settings">
          <h4>The Condition:</h4>
          <div className="Query"/>
            <form onChange={this.handle_condition_change}>
              <label onMouseEnter={this.handle_mouse_enter_condition} onMouseLeave={this.handle_mouse_leave_condition}>
                <input type="radio" name="condition" value="1"/>Has Condition
              </label><br/>
              <label onMouseEnter={this.handle_mouse_enter_condition} onMouseLeave={this.handle_mouse_leave_condition}>
                <input type="radio" name="condition" value="-1"/>No Condition
              </label><br/>
              <label onMouseEnter={this.handle_mouse_enter_condition} onMouseLeave={this.handle_mouse_leave_condition}>
                <input type="radio" name="condition" value="0"/>Either Condition
              </label><br/>
            </form>
          <div className="Query"/>
          <h4>The Test:</h4>
          <div className="Query"/>
            <form onChange={this.handle_test_change}>
              <label onMouseLeave={this.handle_mouse_leave_test} onMouseEnter={this.handle_mouse_enter_test}>
                <input type="radio" name="test" value="1"/>Positive Test
              </label><br/>
              <label onMouseLeave={this.handle_mouse_leave_test} onMouseEnter={this.handle_mouse_enter_test}>
                <input type="radio" name="test" value="-1"/>Negative Test
              </label><br/>
              <label onMouseLeave={this.handle_mouse_leave_test} onMouseEnter={this.handle_mouse_enter_test}>
                <input type="radio" name="test" value="0" defaultChecked="checked"/>Either Test
              </label><br/>
            </form>
          <div className="Query"/>
        </div>
        <div className="Vis">
          <FrequencyChart
            {...data}
            test={this.state.test}
            condition={this.state.condition}
          />
        </div>
      </div>
    );
  }
}