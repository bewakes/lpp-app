import React from 'react';

interface Rational {
    denominator: number;
    numerator: number;
}

interface Step {
    status: ["Feasible" | "Infeasible", "Optimal" | "NonOptimal"];
    zValue: Rational;
    action: "Maximize" | "Minimize";
    basicVariables: string[];
    allVariables: string[];
    currentMatrix: Rational[][];
    currentZRow: Rational[];
}

export type Solution = Step[];

interface SolutionViewProps {
    solution: Solution | undefined;
}

const RationalNumber: React.FC<{value: Rational}> = ({value}) => {
    if (value.denominator == 1) return <>{value.numerator}</>;
    const { numerator, denominator } = value;
    const numClassname = numerator.toString().length > denominator.toString().length ? 'bottom-bordered': '';
    const denClassname = numerator.toString().length <= denominator.toString().length ? 'top-bordered': '';
    return (
        <div className="rational">
            <span className={"numerator "+numClassname}>{denominator < 0 ? -numerator : numerator}</span>
            <span className={"denominator " + denClassname}>{Math.abs(denominator)}</span>
        </div>
    );
};

const Equality: React.FC<{variable: string; value: Rational}> = ({variable, value}) => {
    return (
        <div className="equality-value">
            <b>{variable}</b> <span>=</span><RationalNumber value={value} />;
        </div>
    );
};


const StepView: React.FC<{data: Step; iteration: number;}> = ({data, iteration}) => {
    return (
        <div className="step">
            <h3>Iteration {iteration}</h3>
            <table style={{marginBottom: "10px"}}>
                <thead>
                    <tr>
                        <th>{" "}</th>
                        { data.allVariables.map(x => <th key={x}>{x}</th>) }
                        <th>Solution</th>
                    </tr>
                </thead>
                <tbody>
                    <tr>
                        <th>Z</th>
                        {
                        data.currentZRow.map((c, i) => <td key={i}><RationalNumber value={c} /></td>)
                        }
                    </tr>
                    { data.currentMatrix.map((mat, i) => (
                        <tr key={i}>
                            <th>{data.basicVariables[i]}</th>
                            {
                            mat.map((v, j) => <td key={j}><RationalNumber value={v} /></td>)
                            }
                        </tr>

                    ))
                    }
                </tbody>
            </table>
        </div>
    );
};

const SolutionView: React.FC<SolutionViewProps> = ({solution}) => {
    if(!solution) return null;
    const [showDetail, setShowDetail] = React.useState(false);
    const len = solution.length;
    const lastStep = solution[len - 1];
    const basicValues = lastStep.currentMatrix.map(x => x[x.length-1]);
    const varsValues = lastStep.basicVariables.map((v, i) => ({variable: v, value: basicValues[i]}));
    return (
        <div id="solution">
            <h2>Solution</h2>
            <Equality variable={"Z"} value={lastStep.zValue} />
            {varsValues.map((v, i) => <Equality key={i} {...v}/>)}
            <div>
                <input type="checkbox" checked={showDetail} onClick={(ev) => setShowDetail(!showDetail)} /> Show Detail
            </div>
            <div>
            {
                showDetail && solution.map((step, i) => (
                    <StepView key={i} data={step} iteration={i} />
                ))
            }
            </div>
        </div>
    );
};

export default SolutionView;
