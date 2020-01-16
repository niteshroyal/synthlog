import * as React from 'react';
import {FocusZone, FocusZoneDirection} from 'office-ui-fabric-react/lib/FocusZone';
import {List} from 'office-ui-fabric-react/lib/List';
import {Label, Toggle} from "office-ui-fabric-react";

export default class PredictionsViewer extends React.Component {
    constructor(props, context) {
        super(props, context);
        this.state = {}
    }

    render() {
        console.log("Predictions", this.props.predictions);

        const predictionsByOrigin = new Map();
        this.props.predictions.forEach((value) => {
            const provenance = value.provenance;

            if(!predictionsByOrigin.has(provenance[0])) {
                predictionsByOrigin.set(provenance[0], new Map());
            }
            const predictionsByGroup = predictionsByOrigin.get(provenance[0]);
            if(!predictionsByGroup.has(provenance[1])) {
                predictionsByGroup.set(provenance[1], []);
            }
            predictionsByGroup.get(provenance[1]).push(value);
        });

        console.log("Predictions (grouped)", predictionsByOrigin);

        const renderOrigin = function(origin, predictionsByGroup) {
            return (<div className="prediction-origin" key={origin}>
                <h4>{origin}</h4>
                {Array.from(predictionsByGroup).map(([key, value]) => {
                    return renderGroup(key, value);
                })}
            </div>)
        };

        const renderGroup = function(group, predictions) {
            return (<div className="prediction-group" key={group}>
                {predictions[0].confidence}: {predictions.map(renderPrediction)}
            </div>)
        };

        const renderPrediction = function(prediction) {
            const address = prediction.coordinate.address;
            const key = `${prediction.provenance[0]} ${prediction.provenance[1]} ${address}`;
            return (<span className="prediction" key={key}>
                {address} ({prediction.value})
            </span>)
        };

        return (
            <div id="predictions">
                <h3>Predictions</h3>
                <FocusZone direction={FocusZoneDirection.vertical}>
                    <div>
                        {Array.from(predictionsByOrigin, ([key, value]) => {
                            return renderOrigin(key, value);
                    })}
                    </div>
                </FocusZone>
            </div>
        );
    }
}