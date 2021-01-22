import React from 'react';

import SolutionView, { Solution } from './SolutionView';

interface SolverProps {
}

interface Constraint {
    lhs: number[];
    sign: "Lte" | "Gte";
    rhs: number;
}

const sol = [
    {
        "status":["Feasible","NonOptimal"],
        "basicVariables":["s1"],
        "currentMatrix":[
            [{"denominator":1,"numerator":1},{"denominator":1,"numerator":1},{"denominator":1,"numerator":1},{"denominator":1,"numerator":3}]],"action":"Maximize","zValue":{"denominator":1,"numerator":0},"allVariables":["x1","x2","s1"],"currentZRow":[{"denominator":1,"numerator":-1},{"denominator":1,"numerator":-1},{"denominator":1,"numerator":0},{"denominator":1,"numerator":0}]},{"status":["Feasible","Optimal"],"basicVariables":["x1"],"currentMatrix":[[{"denominator":1,"numerator":1},{"denominator":1,"numerator":1},{"denominator":1,"numerator":1},{"denominator":1,"numerator":3}]],"action":"Maximize","zValue":{"denominator":1,"numerator":3},"allVariables":["x1","x2","s1"],
                "currentZRow":[{"denominator":1,"numerator":0},{"denominator":1,"numerator":0},{"denominator":1,"numerator":1},{"denominator":1,"numerator":3}]}];


const defaultData = {
    decisionVariables: ["x1", "x2", "x3", "x4"],
    objectiveAction: "Maximize",
    objectiveFunction: [7.7, 10.4, 5.2, 12.3],
    constraints: [
        {"lhs": [1, 1, 1, 1], "rhs": 120000, "sign": "Lte"},
        {"lhs": [0, 0, 1, 1], "rhs": 40000 , "sign": "Lte"},
        {"lhs": [1, 0, 1, 0], "rhs": 80000 , "sign": "Lte"},
        {"lhs": [0, 1, 0, 1], "rhs": 50000 , "sign": "Lte"}
    ],
    signRestrictions: ["Positive", "Positive", "Positive", "Positive"]
};

type Action = "Maximize" | "Minimize";

const Solver: React.FC<SolverProps> = () => {
    const [numVars, setNumVars] = React.useState(2);
    const [action, setAction] = React.useState<Action>("Maximize");
    const [coefficients, setCoefficients] = React.useState([1, 1]);
    const [constraints, setConstraints] = React.useState<Constraint[]>([]);
    const [pending, setPending] = React.useState(false);
    const [solution, setSolution] = React.useState<Solution | undefined>();

    const prepareData = () => ({
        decisionVariables: [...new Array(numVars)].map((_, i) => 'x'+(i+1)),
        objectiveAction: action,
        objectiveFunction: coefficients,
        constraints,
        signRestrictions: [...new Array(numVars)].map(_ => "Positive")
    });

    const requestSolve = () => {
        // TODO: validate
        const data = prepareData();
        setPending(true);
        // api call
        fetch(
            'https://bewakes.com/lpp',
            // 'http://localhost:4000/lpp',
            {
                method: 'POST',
                mode: 'cors',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(data),
            },
        )
            .then(r => r.json())
            .then((d: Solution) => { setSolution(d); setPending(false); });
    };

    React.useEffect(() => {
        const coeffsDiff = numVars - coefficients.length;
        if (coeffsDiff == 0) {
            return;
        }
        if (coeffsDiff < 0) {
            setCoefficients(coefficients.slice(0, numVars));
            setConstraints(constraints.map(c => ({...c, lhs: c.lhs.slice(0, numVars)})));
        } else {
            setCoefficients([...coefficients, ...[new Array(coeffsDiff)].map(_ => 0)]);
            setConstraints(constraints.map(c => ({...c, lhs: [...c.lhs, ...[new Array(coeffsDiff)].map(_=> 0)] })));
        }
    }, [numVars]);

    const setNthCoefficient = (n) => (ev) => {
        setCoefficients(coefficients.map((c, i) => i == n ? ev.target.value : c));
    };

    const addConstraint = () => {
        setConstraints([...constraints, {lhs: [...new Array(numVars)].map(_=>1), sign: 'Lte', rhs: 3}]);
    };

    const setNthConstraintJthCoefficient = (n, j) => ev => {
        const newConstraints = constraints.map(
            (c, i) => i != n
            ? c
            : ({
                rhs: c.rhs,
                sign: c.sign,
                lhs:  c.lhs.map((e, jj) => jj == j ? parseFloat(ev.currentTarget.value) : e)
            })
        );
        setConstraints(newConstraints);
    };

    const removeIthConstraint = (i) => () => {
        setConstraints(constraints.filter((c, ii) => i != ii));
    };

    const setNthConstraintRhs = (n) => ev => {
        const newConstraints = constraints.map(
            (c, i) => i == n ? ({...c, rhs: parseFloat(ev.currentTarget.value)}): c
        );
        setConstraints(newConstraints);
    };

    return (
        <div id="solver">
            <table>
                <tbody>
                    <tr>
                        <td>
                            <select name="action" value={action} onChange={e => setAction(e.target.value)}>
                                <option value="Maximize">Maximize</option>
                            </select>
                        </td>
                    </tr>
                    <tr>
                        <td>Number of variables: </td>
                        <td>
                            <input
                                type="number"
                                value={numVars}
                                onChange={ev => setNumVars(parseInt(ev.currentTarget.value))}
                                disabled={pending}
                            />
                        </td>
                    </tr>
                    <tr>
                        <td>Z Coefficients <b>(c1x1 + c2x2 + ...)</b></td>
                        <td className="coefficients-container">
                            { coefficients.map((c, i) => (
                            <input
                                key={i}
                                type="number"
                                value={coefficients[i]}
                                onChange={setNthCoefficient(i)}
                                disabled={pending}
                            />
                            ))}
                        </td>
                    </tr>
                    <tr> <td>&nbsp;</td> </tr>
                    <tr> <td>&nbsp;</td> </tr>
                    <tr> <td>&nbsp;</td> </tr>
                    <tr>
                        <td>Constraints</td>
                        <td>
                            <div>
                                <div className="constraints-list">
                                    { constraints.map((c, i) => (
                                        <div key={i}>
                                            {c.lhs.map((l, j) => (
                                                <input
                                                    key={j}
                                                    type="number"
                                                    value={l}
                                                    onChange={setNthConstraintJthCoefficient(i, j)}
                                                    disabled={pending}
                                                />
                                            ))}
                                            <b> &lt;= </b>
                                            <input
                                                type="number"
                                                value={c.rhs}
                                                onChange={setNthConstraintRhs(i)}
                                                disabled={pending}
                                            />
                                            <button type="button" onClick={removeIthConstraint(i)} disabled={pending}>X</button>
                                        </div>
                                    ))
                                    }
                                </div>
                                <button type="button" onClick={addConstraint} disabled={pending}>Add Constraint</button>
                            </div>
                        </td>
                    </tr>
                    <tr> <td>&nbsp;</td> </tr>
                    <tr> <td>&nbsp;</td> </tr>
                    <tr>
                        <td>
                            <button type="button" onClick={requestSolve} disabled={pending || constraints.length < 1}>Solve!</button>
                        </td>
                    </tr>
                </tbody>
            </table>
            { solution && <SolutionView solution={solution} /> }
        </div>
    );
};

export default Solver;
