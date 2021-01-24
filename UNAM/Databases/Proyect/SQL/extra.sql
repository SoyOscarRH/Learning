create or replace procedure new_manager(
   old_manager_curp CHAR(18),
   new_manager_curp CHAR(18) 
)
language plpgsql    
as $$
begin

    -- inserting new
    INSERT INTO manager(curp, id_establishment, rfc, ssn) 
    SELECT curp, id_establishment, rfc, ssn 
      FROM general WHERE general.curp = new_manager_curp;

    INSERT INTO general(curp, id_establishment, rfc, ssn) 
    SELECT curp, id_establishment, rfc, ssn 
      FROM manager WHERE manager.curp = old_manager_curp;

    -- deleting old
    DELETE FROM manager WHERE curp = old_manager_curp;
    DELETE FROM general WHERE curp = new_manager_curp;

    commit;
end;$$


create or replace procedure new_manager(
   old_manager_curp CHAR(18),
   new_manager_curp CHAR(18) 
)
language plpgsql    
as $$
begin

    -- inserting new
    INSERT INTO manager(curp, id_establishment, rfc, ssn) 
    SELECT curp, id_establishment, rfc, ssn 
      FROM general WHERE general.curp = new_manager_curp;

    INSERT INTO general(curp, id_establishment, rfc, ssn) 
    SELECT curp, id_establishment, rfc, ssn 
      FROM manager WHERE manager.curp = old_manager_curp;

    -- deleting old
    DELETE FROM manager WHERE curp = old_manager_curp;
    DELETE FROM general WHERE curp = new_manager_curp;

    commit;
end;$$

-- example select * from manager where id_establishment = 1;


CREATE OR REPLACE FUNCTION can_client_buy_it(client CHAR(18), drug INTEGER, requested_dosis SMALLINT) RETURNS bool AS $$
  BEGIN
      RETURN EXISTS (
        SELECT 1 FROM indicates, prescription, consultation
        WHERE
          indicates.dosis >= requested_dosis AND
          indicates.id_drug >= drug AND
          indicates.invoice = prescription.invoice AND
          prescription.id_consultation = consultation.id_consultation AND
          consultation.patient = client);
  END;
$$ LANGUAGE plpgsql;

-- SELECT can_client_buy_it('KTPU069372DUAGI194'::CHAR(18), 264::INTEGER, 1::SMALLINT);

/* SELECT indicates.id_drug, consultation.patient, indicates.dosis FROM indicates, prescription, consultation
        WHERE
          indicates.invoice = prescription.invoice AND
          prescription.id_consultation = consultation.id_consultation
		  
		GROUP BY
			indicates.id_drug, consultation.patient, indicates.dosis;
  */