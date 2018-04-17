import React, { Component } from 'react';
import './FrequencyChart.css';
import person from './person.svg';
import * as d3 from 'd3'; 

/**
 * props: 
 *  has_condition: float
 *  positive_condition: float
 *  positive_no_condition: float
 *  number_of_people: count
 *  test: -1, 0, 1: 0 if not known
 *  condition: -1, 0, 1: 0 if not known
 *  
 */
export class FrequencyChart extends Component {
  d3id = "D3Vis";
  data = []

  // necessary part for getDerivedStateFromProps
  state = {
    condition: 0, 
    test: 0,
  }
  

  defaults = {
    height: 200, width: 1200, margin : {
      top: 20, right: 120, bottom: 20, left: 250,
    }
  }
  componentDidMount() {
    this.renderD3(); 
  }

  // getting 'nextProps' from 'prevState'
  static getDerivedStateFromProps(nextProps, prevState) {
    // where TEST = 0
    if (nextProps.test === 0) {
      if (nextProps.condition === 1) {
        return {
          condition: 1,
          test: 0
        };
      }
      else if (nextProps.condition === 0) {
        return {
          condition: 0,
          test: 0
        };
      }
      else {
        return {
          condition: -1,
          test: 0
        };
      }
    }
    // where TEST = 1
    else if (nextProps.test === 1) {
      if (nextProps.condition === 1) {
        return {
          condition: 1,
          test: 1
        };
      }
      else if (nextProps.condition === 0) {
        return {
          condition: 0,
          test: 1
        };
      }
      else {
        return {
          condition: -1,
          test: 1
        };
      }
    }
    // where TEST = -1
    else {
      if (nextProps.condition === 1) {
        return {
          condition: 1,
          test: -1
        };
      }
      else if (nextProps.condition === 0) {
        return {
          condition: 0,
          test: -1
        };
      }
      else {
        return {
          condition: -1,
          test: -1
        };
      }
    }
    return null;
  }

  componentDidUpdate(prevProps, prevState) {
    // labels that should be rendered: 
    let new_labels = this.make_data().map(el => el.label); 
    let all_labels = this.data.map(el => el.label);
    let chartHeight = this.defaults.height - this.defaults.margin.top - this.defaults.margin.bottom; 
    let labelHeight = chartHeight / 4; 

    setTimeout(() => {
      d3.selectAll('.attribute')
        .transition()
        .duration(500)
        .attr('transform', (d, i) => {
          let ind = new_labels.indexOf(d.label); 
          if(ind === -1) {
            return `translate(0, ${800})`;
          }
          return `translate(0, ${labelHeight * (ind + .5)})`;
        });
    }, 10);
    // setTimeout(() => {
    //   this.renderD3(); 
    // }, 1000);
  }

  renderD3 = () => {
    console.log("Clearing and rendering d3");
    let chartWidth = this.defaults.width- this.defaults.margin.left - this.defaults.margin.right; 
    let chartHeight = this.defaults.height - this.defaults.margin.top - this.defaults.margin.bottom; 
    let containerId = '#' + this.d3id; 



    d3.select(containerId).html(''); // just clear out what is there

    var chart = d3.select(containerId).append('svg')
      .attr('width', this.defaults.width)
      .attr('height', this.defaults.height)
      .attr('id', 'chart')
      .append('g')
        .attr('transform', `translate(${this.defaults.margin.left}, ${this.defaults.margin.top})`);


    // set up axis
    this.data = this.make_data(); 
    let data = this.data; 

    // for each label, put it on the left of the line (using margin)

    let labelHeight = chartHeight / 4; 

    let attributes = chart.selectAll('.attribute')
      .data(data)
      .enter() 
      .append('g')
      .attr('class', 'attribute')
      .attr('id', 'groupdata')
      .attr('transform', (_, i) => {
        return `translate(0, ${labelHeight * (i + .5)})`;
      });

      // let iconOffset = chartWidth / Math.max(...data.map(d => d.count)) 
      let iconOffset = Math.min(...[20, chartWidth / Math.max(...data.map(d => d.count))])
      attributes.selectAll('circle')
        .data((d) => [...Array(d.count).keys()])
        .enter()
        // <image x="10" y="20" width="80" height="80" xlink:href="recursion.svg" />
        .append('image') // change to icon
        .attr('id', 'human')
        .attr('x', (_, i) => (1 + i) * iconOffset)
        .attr('y', (_, i) => -15)
        .attr('width', 30)
        .attr('height', 30)
        .attr('xlink:href', person)
        .attr('class', function(d, _, i) {
          //console.log(d)
          //console.log(_)
          //console.log(i)
        });
      
      attributes
        .append('g')
        .attr('transform', `translate(-10, 5)`)
        .append('text')
        .attr('class', 'test_label')
        .attr('id', 'idlabel')
        .attr('text-anchor', 'end')
        .text((d) => d.label);

      attributes
        .append('g')
        .attr('transform', d => `translate(${iconOffset * (d.count + 2)}, 5)`)
        .append('text')
        .attr('id', 'idcount')
        .text(d => d.count);



    chart.append('line')
      .attr('x1', '0')
      .attr('x2', '0')
      .attr('y1', '0')
      .attr('y2', chartHeight)
      .attr('stroke-width', 2)
      .attr('stroke', 'black');
  }

  make_data = () => {
    let number = this.props.number_of_people;
    let has_condition = Math.round(this.props.has_condition * number),
        no_condition = Math.round((1 - this.props.has_condition) * number),
        positive_condition = Math.round(this.props.positive_condition * has_condition), 
        negative_condition = Math.round((1 - this.props.positive_condition) * has_condition), 
        positive_no_condition = Math.round(this.props.positive_no_condition * no_condition), 
        negative_no_condition = Math.round((1 - this.props.positive_no_condition) * no_condition);
      
    let data  = [], labels = []; 
    if(this.props.test === 0) {
      if(this.props.condition === 0) {
        labels = [
          "Positive Test & Has Condition",
          "Positive Test & No Condition",
          "Negative Test & Has Condition",
          "Negative Test & No Condition", 
        ]; 
        data = [
          positive_condition, 
          positive_no_condition, 
          negative_condition, 
          negative_no_condition
        ]
      } else if (this.props.condition === 1) {
        labels = [
          "Positive Test & Has Condition",
          "Negative Test & Has Condition",
        ]; 
        data = [
          positive_condition, 
          negative_condition, 
        ]
      } else {
        labels = [
          "Positive Test & No Condition",
          "Negative Test & No Condition", 
        ]; 
        data = [
          positive_no_condition, 
          negative_no_condition
        ]
      }
    } else if (this.props.test === 1) {
      if(this.props.condition === 0) {
        labels = [
          "Positive Test & Has Condition",
          "Positive Test & No Condition",
        ]; 
        data = [
          positive_condition, 
          positive_no_condition, 
        ]
      } else if (this.props.condition === 1) {
        labels = [
          "Positive Test & Has Condition",
        ]; 
        data = [
          positive_condition, 
        ]
      } else {
        labels = [
          "Positive Test & No Condition",
        ]; 
        data = [
          positive_no_condition, 
        ]
      }

    } else {
      if(this.props.condition === 0) {
        labels = [
          "Negative Test & Has Condition",
          "Negative Test & No Condition", 
        ]; 
        data = [
          negative_condition, 
          negative_no_condition
        ]
      } else if (this.props.condition === 1) {
        labels = [
          "Negative Test & Has Condition",
        ]; 
        data = [
          negative_condition, 
        ]
      } else {
        labels = [
          "Negative Test & No Condition", 
        ]; 
        data = [
          negative_no_condition
        ]
      }
    }

    let d = [];
    for (var i = 0; i < labels.length; i++) {
      d.push({label: labels[i], count: data[i]})
    }

    return d;

  }

  /**
   * This just creates a div + element to hold d3 svg things
   */
  render() {
    return (
        <div className="FrequencyChart">
          <h2>Total: {this.props.number_of_people}</h2>
          <div id={this.d3id}/>
        </div>
    );
  }
}