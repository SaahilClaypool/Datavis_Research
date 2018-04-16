import React, { Component } from 'react';
import './Question.css';
import {now} from 'd3';

/**
 * props: 
 *  - Question to render
 *  - function: save(guessA, guessB, data)
 *  - data
 * 
 */
export class Question extends Component {

    questionN = 100; 
    // TODO
    visType = "Interactive"

    constructor(props) {
        super(props); 
        if(props.QuestionNumber) {
            this.questionN = props.QuestionNumber; 
        }
        this.state = {
            ready: false
        }
        console.log("RENDERING THE QUESTION AGAIN");
    }

    static getDerivedStateFromProps(nextProps, prevState) {
        return {
            ready: false
        }
    }

    handleSubmit = (e) => {
        e.preventDefault(); 
        let guessElementA = document.getElementById("guessA");
        let guessA = NaN;
        if (guessElementA)   {
            guessA = parseInt(guessElementA.value, undefined); 
        }
        let guessElementB = document.getElementById("guessB");
        let guessB = NaN; 
        if (guessElementB)   {
            guessB = parseInt(guessElementB.value, undefined); 
        }
        if(isNaN(guessB) || isNaN(guessA)) {
            console.log(guessA)
            console.log(guessB)
            return; 
        }
        this.props.nextScreen( guessA, guessB, this.props.data, 
            this.props.visType, this.questionN, now() - this.state.start); 
    }

    ready = (e) => {
        this.setState ({
            ready: true, 
            start: now()
        });
    }

    render() {

        let form = (  <form onSubmit={(e) => this.handleSubmit(e)}>
                    <h4>Imagine {this.questionN} people are tested for the disease. </h4>
                    <p>
                        <i>How many people do you think will test positive?</i>
                    </p>
                    <label>Number testing Positive: 
                       <input type="number" min="0" max="100" id="guessA" placeholder={"Enter number..."} required={true}/>
                    </label>
                    <br/>
                    <p>
                        <i>Out of those people, how many will have the disease?</i>
                    </p>
                    <label>Number with Disease:
                        <input type="number" min="0" max="100" id="guessB" placeholder={"Enter number..."} required={true}/>
                    </label>
                    <br/>
                    <input type="submit" value="Next" />
                </form>); 
            
        let footer = (<div>
                        <p>Click "Ready" to answer questions about this problem </p>
                        <button onClick={this.ready}>Ready</button>
                      </div>);
        if( this.state.ready) {
            footer = form; 
        }

        return (
            <div className="Question">
                <p>There is a newly discovered disease, Disease X, which is
                transmitted by a bacterial infection found in the population.
                There is a test to detect whether or not a person has the
                disease, but it is not perfect. Here is some information
                about the current research on Disease X and efforts to test
                for the infection that causes it. </p>
                {/* The children are the vis, passed through*/}
                {this.props.children}
                <hr/>
                {footer}
            </div>
        );
    }
}