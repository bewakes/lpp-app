import React from 'react';

interface SolverProps {
}

interface Constraint {
    lhs: number[];
    sign: "Lte" | "Gte";
    rhs: number;
}

const Solver: React.FC<SolverProps> = () => {
    const [numVars, setNumVars] = React.useState(2);
    const [coefficients, setCoefficients] = React.useState([1, 1]);
    const [constraints, setConstraints] = React.useState<Constraint[]>([]);
    const [pending, setPending] = React.useState(false);

    const requestSolve = () => {
        setPending(true);
        // api call
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
            </table>
        </div>
    );
};

export default Solver;
