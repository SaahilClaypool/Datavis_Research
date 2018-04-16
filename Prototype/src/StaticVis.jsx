import React, { Component } from 'react';
import person from './person.svg';
import * as d3 from 'd3'; 

export class StaticVis extends Component {
    d3id = "StaticVisID";
    defaults = {
        height: 200, width: 1200, margin: {
            top: 20, right: 120, bottom: 20, left: 250,
        }
    }
    componentDidMount() {
        this.renderD3(); 
    }
    componentDidUpdate() {
        this.renderD3();
    }

    renderD3 = () => {
        let number = this.props.number_of_people;
        let has_condition = Math.round(this.props.has_condition * number),
            no_condition = Math.round((1 - this.props.has_condition) * number),
            positive_condition = Math.round(this.props.positive_condition * has_condition), 
            negative_condition = Math.round((1 - this.props.positive_condition) * has_condition), 
            positive_no_condition = Math.round(this.props.positive_no_condition * no_condition), 
            negative_no_condition = Math.round((1 - this.props.positive_no_condition) * no_condition),
            total = this.props.number_of_people; 
        
        let rows = 4; 
        let cols = Math.ceil(number / rows); 
        let chartWidth = this.defaults.width - this.defaults.margin.left - this.defaults.margin.right;
        let chartHeight = this.defaults.height - this.defaults.margin.top - this.defaults.margin.bottom;
        let containerId = '#' + this.d3id;
        d3.select(containerId).html(''); // just clear out what is there


        var chart = d3.select(containerId).append('svg')
            .attr('width', this.defaults.width)
            .attr('height', this.defaults.height)
            .attr('id', 'chart')
            .append('g')
            .attr('transform', `translate(${this.defaults.margin.left}, ${this.defaults.margin.top})`);

        
        let coloffset = Math.floor(chartWidth / (number / rows)); 
        let rowoffset = Math.floor(chartHeight / rows);
        
        let d = [...Array(number).keys()]
        console.log(rowoffset)

        let getX = (i) => ((1 + i) % cols) * coloffset; 
        let getY = (i) => (Math.floor(i / cols) * rowoffset); 
        console.log(`cols: ${cols}, coloffset: ${coloffset}, rowoff: ${rowoffset}`)

        d.forEach(i => {
            console.log(i)
            console.log(getX(i));
            console.log(getY(i));
            console.log("")
        })

        chart.selectAll('circle')
            .data([...Array(number).keys()])
            .enter()
            .append('image')
            .attr('x', (_, i) => getX(i))
            .attr('y', (_, i) => getY(i))
            .attr('width', 30)
            .attr('height', 30)
            .attr('xlink:href', person)
        
        // rect for people with disease
        // max half of cols
        chart.append('rect')
            .attr('x', 0)
            .attr('y', 0)
            .attr('fill', 'black')
            .attr('width', has_condition * coloffset)
            .attr('height', rowoffset)
            .attr('opacity', 0.5); 

        chart.append('rect')
            .attr('x', coloffset * (negative_condition))
            .attr('y', 0)
            .attr('stroke', 'black')
            .attr('stroke-width', 2)
            .attr('width', (2)  * coloffset)
            .attr('height', rowoffset * 2)
            .attr('fill-opacity', 0);



    }

    render() {
        return (
            <div className="StaticVis">
                <div id="StaticVisID"/>
            </div>
        );
    }
}