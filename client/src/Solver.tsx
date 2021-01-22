import React from 'react';

import SolutionView, { Solution } from './SolutionView';

interface SolverProps {
}

interface Constraint {
    lhs: string[];
    sign: "Lte" | "Gte";
    rhs: string;
}

type Action = "Maximize" | "Minimize";

const nCopiesOf = (n: number, a: any) => {
    const arr = [];
    for(let x=0;x<n;x++) arr.push(a);
    return arr;
};

const Solver: React.FC<SolverProps> = () => {
    const [numVars, setNumVars] = React.useState("2");
    const [action, setAction] = React.useState<Action>("Maximize");
    const [coefficients, setCoefficients] = React.useState(["1", "1"]);
    const [constraints, setConstraints] = React.useState<Constraint[]>([]);
    const [pending, setPending] = React.useState(false);
    const [solution, setSolution] = React.useState<Solution | undefined>();

    const prepareData = () => ({
        decisionVariables: [...new Array(parseInt(numVars))].map((_, i) => 'x'+(i+1)),
        objectiveAction: action,
        objectiveFunction: coefficients.map(parseFloat),
        constraints: constraints.map(c => ({...c, lhs: c.lhs.map(parseFloat), rhs: parseFloat(c.rhs)})),
        signRestrictions: nCopiesOf(parseInt(numVars), "Positive")
    });

    const requestSolve = () => {
        // TODO: validate
        const data = prepareData();
        setPending(true);
        // api call
        fetch(
            'https://bewakes.com/lpp-api',
            {
                method: 'POST',
                mode: 'cors',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(data),
            },
        )
            .then(r => r.json())
            .then((d: Solution) => { setSolution(d); setPending(false); })
            .catch(_ => {setPending(false); alert('Something went wrong. Please check if all data are valid and nonempty and try again');});
    };

    React.useEffect(() => {
        const parsed = parseInt(numVars);
        if(isNaN(parsed)) return;
        console.warn({parsed});

        const coeffsDiff = parsed - coefficients.length;
        if (coeffsDiff == 0) {
            console.warn('coeffssame');
            return;
        }
        if (coeffsDiff < 0) {
            setCoefficients(coefficients.slice(0, parsed));
            setConstraints(constraints.map(c => ({...c, lhs: c.lhs.slice(0, parsed)})));
        } else {
            setCoefficients([...coefficients, ...nCopiesOf(coeffsDiff, "0")]);
            setConstraints(constraints.map(c => ({...c, lhs: [...c.lhs, ...nCopiesOf(coeffsDiff, "0")] })));
        }
    }, [numVars]);

    const setNthCoefficient = (n) => (ev) => {
        setCoefficients(coefficients.map((c, i) => i == n ? ev.target.value : c));
    };

    const addConstraint = () => {
        setConstraints([...constraints, {lhs: nCopiesOf(parseInt(numVars), "1"), sign: 'Lte', rhs: "0"}]);
    };

    const setNthConstraintJthCoefficient = (n, j) => ev => {
        const newConstraints = constraints.map(
            (c, i) => i != n
            ? c
            : ({
                rhs: c.rhs,
                sign: c.sign,
                lhs:  c.lhs.map((e, jj) => jj == j ? ev.currentTarget.value : e)
            })
        );
        setConstraints(newConstraints);
    };

    const removeIthConstraint = (i: number) => () => {
        setConstraints(constraints.filter((c, ii) => i != ii));
    };

    const setNthConstraintRhs = (n: number) => ev => {
        const newConstraints = constraints.map(
            (c, i) => i == n ? ({...c, rhs: ev.currentTarget.value}): c
        );
        setConstraints(newConstraints);
    };

    const setVariableNumber = ev => {
        setNumVars(ev.currentTarget.value);
    };

    return (
        <div id="solver">
            <table>
                <tbody>
                    <tr>
                        <td>
                            <select name="action" value={action} onChange={e => setAction(e.target.value as Action)}>
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
                                onChange={setVariableNumber}
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
                                type="text"
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
                                                    type="text"
                                                    value={l}
                                                    onChange={setNthConstraintJthCoefficient(i, j)}
                                                    disabled={pending}
                                                />
                                            ))}
                                            <b> &lt;= </b>
                                            <input
                                                type="text"
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
            { !pending && solution && <SolutionView solution={solution} /> }
        </div>
    );
};

export default Solver;
