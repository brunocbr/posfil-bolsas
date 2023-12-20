# Distribuição de Bolsas - PPG Filosofia PUC-SP

Este repositório contém um programa desenvolvido para realizar a distribuição de bolsas do Programa de Pós-Graduação em Filosofia da PUC-SP. O código, implementado em SWI-Prolog, utiliza critérios como nível acadêmico, cota racial, agência financiadora e modalidade para efetuar a distribuição justa e transparente das bolsas entre os candidatos.

## Utilização

### Pré-requisitos

Certifique-se de ter o SWI-Prolog instalado em seu ambiente para executar o programa.

### Passos para Execução

1. Clone o repositório para o seu ambiente local:

   ```bash
   git clone https://github.com/brunocbr/posfil-bolsas.git
   ```

2. Acesse o diretório do código fonte no projeto:

   ```bash
   cd posfil-bolsas/src
   ```

3. Execute o SWI-Prolog e carregue o programa:

   ```prolog
   swipl bolsas.pl
   ```

4. Utilize os predicados disponíveis para realizar consultas e distribuição de bolsas.

## Exemplos de Uso

```prolog
% Exemplo: Consultar candidatos de mestrado
?- candidatos(mestrado, ListaMestrado).

% Exemplo: Realizar distribuição de bolsas, lendo dados de arquivos CSV
?- carrega_dados, atribuições_bolsas(Atribuicoes).
```

## Autor

Este programa foi desenvolvido por Bruno Loureiro Conte.

## Licença

Este projeto é distribuído sob a licença [MIT](LICENSE).
