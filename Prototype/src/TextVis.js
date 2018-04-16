import React, { Component } from 'react';
import './TextVis.css';

/**
 * props: 
 */
export class TextVis extends Component {
    render() {
        let number = this.props.number_of_people;
        let has_condition = Math.round(this.props.has_condition * number),
            no_condition = Math.round((1 - this.props.has_condition) * number),
            positive_condition = Math.round(this.props.positive_condition * has_condition), 
            negative_condition = Math.round((1 - this.props.positive_condition) * has_condition), 
            positive_no_condition = Math.round(this.props.positive_no_condition * no_condition), 
            negative_no_condition = Math.round((1 - this.props.positive_no_condition) * no_condition),
            total = this.props.number_of_people; 
        return (
            <div className="TextVis">
               There is a total of {total} in the population. 
               Out of the {total} people in the population, 
               {has_condition} people actually have the
               disease. Out of these {has_condition} people,  {positive_condition} will receive a positive test
               result and {negative_condition} will receive a negative test result. On the other
               hand, {no_condition} people do not have the disease (i.e., they are
               perfectly healthy). Out of these {no_condition} people, {positive_no_condition} will receive a
               positive test result and {negative_no_condition} will receive a negative test
               result
            </div>
        )
    }
}